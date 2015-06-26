{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Network.Docker.Internal (
      Docker
    , DockerT
    , runDocker
    , RequestURI
    , dockerRequest
    , dockerRequest_
    , dockerRequestStream
    , dockerRequestBytes
    ) where

import           Control.Monad.Base (MonadBase)
import           Control.Monad.Catch (MonadThrow)
import           Control.Monad.IO.Class (MonadIO(..))
import           Control.Monad.Reader (ReaderT(..), ask)
import           Control.Monad.Trans.Control (MonadBaseControl(..))
import           Control.Monad.Trans.Resource (ResourceT, MonadResource)
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A
import qualified Data.Attoparsec.ByteString as Atto
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import           Data.Char (isSpace, chr)
import           Data.Conduit ((=$=), ($$+-), ($=+), yield, awaitForever)
import           Data.Conduit (Conduit, ResumableSource)
import qualified Data.Conduit.Attoparsec as CA
import qualified Data.Conduit.List as CL
import           Data.Default.Class (def)
import           Data.Maybe (fromMaybe)
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.X509.CertificateStore (CertificateStore, makeCertificateStore)
import           Data.X509.File (readSignedObject)
import           Data.X509.Validation (FailedReason(..), validateDefault)
import           Network.Connection (TLSSettings(..))
import           Network.HTTP.Conduit (Manager, mkManagerSettings, withManagerSettings)
import           Network.HTTP.Conduit (Request(..), Response(..), parseUrl, http)
import           Network.TLS (ClientParams(..), ClientHooks(..))
import           Network.TLS (Credential)
import           Network.TLS (Supported(..), Shared(..))
import           Network.TLS (defaultParamsClient, credentialLoadX509)
import           Network.TLS.Extra (ciphersuite_all)
import           Network.URI (URI(..), URIAuth(..), parseAbsoluteURI)
import           System.Environment (lookupEnv)
import           System.FilePath ((</>))

------------------------------------------------------------------------

type RequestURI = Text

type HostName = String
type Port     = String

data DockerState = DockerState {
      dockerHost        :: HostName
    , dockerPort        :: Port
    , dockerCAStore     :: CertificateStore
    , dockerCredentials :: Credential
    , dockerManager     :: Manager
    }

newtype DockerT m a = DockerT { runDockerT :: ReaderT DockerState (ResourceT m) a }
    deriving (Monad, Applicative, Functor)

type Docker = DockerT IO

deriving instance MonadIO m     => MonadIO (DockerT m)
deriving instance MonadThrow m  => MonadThrow (DockerT m)
deriving instance MonadBase b m => MonadBase b (DockerT m)

deriving instance (MonadThrow m, MonadBase IO m, MonadIO m, Applicative m) => MonadResource (DockerT m)

------------------------------------------------------------------------

runDocker :: (MonadIO m, MonadBaseControl IO m) => DockerT m a -> m a
runDocker dio = do
    (host, port)   <- lookupDockerHost
    (store, creds) <- lookupDockerCerts
    tlsSettings    <- mkTlsSettings host port store creds

    let settings = mkManagerSettings tlsSettings Nothing
    withManagerSettings settings $ \manager -> do
        let state = DockerState host port store creds manager
        (runReaderT . runDockerT) dio state

dockerRequest :: (MonadIO m, MonadThrow m, MonadBase IO m, A.FromJSON a)
              => RequestURI
              -> (Request -> Request)
              -> DockerT m (Response (DockerT m a))
dockerRequest uri transformReq = do
    response <- dockerRequestBytes uri transformReq
    return (sinkJSON <$> response)

dockerRequest_ :: (MonadIO m, MonadThrow m, MonadBase IO m)
               => RequestURI
               -> (Request -> Request)
               -> DockerT m (Response ())
dockerRequest_ uri transformReq = do
    response <- dockerRequestBytes uri transformReq
    return (const () <$> response)

dockerRequestStream :: (MonadIO m, MonadThrow m, MonadBase IO m, A.FromJSON a)
              => RequestURI
              -> (Request -> Request)
              -> DockerT m (Response (ResumableSource (DockerT m) a))
dockerRequestStream uri transformReq = do
    response <- dockerRequestBytes uri transformReq
    return (streamJSON <$> response)

dockerRequestBytes :: (MonadIO m, MonadThrow m, MonadBase IO m)
              => RequestURI
              -> (Request -> Request)
              -> DockerT m (Response (ResumableSource (DockerT m) B.ByteString))
dockerRequestBytes uri transformReq = do
    DockerState{..} <- DockerT ask

    let url = "https://" ++ dockerHost ++ ":" ++ dockerPort ++ T.unpack uri
    request  <- transformReq <$> parseUrl url

    http request dockerManager

------------------------------------------------------------------------

sinkJSON :: forall m a. (MonadResource m, A.FromJSON a) => ResumableSource m B.ByteString -> m a
sinkJSON src = do
    json <- (src $$+- CA.sinkParser A.json)
    either fail return (A.parseEither A.parseJSON json)

streamJSON :: (MonadResource m, A.FromJSON a) => ResumableSource m B.ByteString -> ResumableSource m a
streamJSON src = (src $=+ conduitJSON)

conduitJSON :: (MonadIO m, MonadResource m, A.FromJSON a) => Conduit B.ByteString m a
conduitJSON =
    CA.conduitParser json =$= CL.map snd =$= awaitForever parse
  where
    json     = A.json <* Atto.skipWhile isSpace8
    isSpace8 = isSpace . chr . fromIntegral
    parse    = either fail yield . A.parseEither A.parseJSON

------------------------------------------------------------------------

mkTlsSettings :: MonadIO m => HostName -> Port -> CertificateStore -> Credential -> m TLSSettings
mkTlsSettings host port caStore creds = return (TLSSettings params)
  where
    params = (defaultParamsClient host (C.pack port)) {
          clientSupported = def {
            supportedCiphers = ciphersuite_all
          }
        , clientShared = def {
            sharedCAStore = caStore
          }
        , clientHooks = def {
            onCertificateRequest = \_ -> return (Just creds)
          , onServerCertificate  = \cs vc sid cc -> do
              problems <- validateDefault cs vc sid cc
              -- allow cert to not have a common name
              return (filter (/= NoCommonName) problems)
          }
        }

------------------------------------------------------------------------

lookupDockerHost :: MonadIO m => m (HostName, Port)
lookupDockerHost = do
    host <- lookupEnv' "DOCKER_HOST"
    case parseAbsoluteURI host >>= uriAuthority of
        Nothing -> error ("DOCKER_HOST was not valid: " ++ host)
        Just x  -> return (uriRegName x, drop 1 (uriPort x))

lookupDockerCerts :: MonadIO m => m (CertificateStore, Credential)
lookupDockerCerts = do
    certPath <- lookupEnv' "DOCKER_CERT_PATH"

    let caFile   = certPath </> "ca.pem"
        certFile = certPath </> "cert.pem"
        keyFile  = certPath </> "key.pem"

    store <- makeCertificateStore <$> liftIO (readSignedObject caFile)
    creds <- either error id <$> liftIO (credentialLoadX509 certFile keyFile)

    return (store, creds)

lookupEnv' :: MonadIO m => String -> m String
lookupEnv' key = fromMaybe (error msg) <$> liftIO (lookupEnv key)
  where
    msg = "Environment variable " ++ key ++ " not set"
