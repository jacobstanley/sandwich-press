{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Network.Docker (
      Docker
    , DockerT
    , runDocker

    , Id(..)
    , Image(..)

    , build

    , HostName
    , DomainName
    , HostAddress
    , Protocol(..)
    , SrcPort(..)
    , DstPort(..)
    , CreateContainer(..)
    , CreateContainerResponse(..)

    , ContainerName
    , Container(..)
    , NetworkSettings(..)
    , IPPrefixLen
    , IPAddress(..)
    , MacAddress(..)

    , create
    , containerFrom
    , start
    , stop

    , getImages
    , getImage
    , getContainer
    ) where

import           Control.Applicative ((<|>), empty)
import           Data.Aeson ((.=), (.:), (.:?))
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import           Data.Conduit ((=$=), ($$+-))
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.List as CL
import           Data.Int (Int64)
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Maybe (fromMaybe, mapMaybe)
import           Data.Set (Set)
import qualified Data.Set as S
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Data.Time (UTCTime)
import           Data.Word (Word16)
import           GHC.TypeLits
import           Network.HTTP.Conduit (Request(..), RequestBody(..), Response(..))
import           Network.HTTP.Types.Header (hContentType)
import           System.IO (stdout)

import           Data.Dockerfile
import           Network.Docker.Internal

------------------------------------------------------------------------

newtype Id = Id { unId :: B.ByteString }
  deriving (Eq, Ord)

type ContainerName = Text

------------------------------------------------------------------------

class Identifier a where
    encodeIdentifier :: a -> Text

instance Identifier Id where
    encodeIdentifier = T.decodeUtf8 . unId

instance Identifier RepoTag where
    encodeIdentifier = encodeRepoTag

instance Identifier ContainerName where
    encodeIdentifier = id

------------------------------------------------------------------------

getImages :: Docker [Image]
getImages = responseBody =<< dockerRequest "/images/json?all=true" id

getImage :: Identifier a => a -> Docker Image
getImage x = responseBody =<< dockerRequest uri id
    -- DRAGONS When a specific image is requested, Docker doesn't
    -- DRAGONS tell us about its repotag's.
  where
    uri = "/images/" <> encodeIdentifier x <> "/json"

getContainer :: Identifier a => a -> Docker Container
getContainer x = responseBody =<< dockerRequest uri id
  where
    uri = "/containers/" <> encodeIdentifier x <> "/json"

------------------------------------------------------------------------

build :: (KnownSymbol repo, KnownSymbol tag) => Dockerfile repo tag -> Docker Id
build dockerfile = do
    response <- dockerRequestStream ("/build" <> query) $ \x -> x {
        method         = "POST"
      , requestHeaders = [(hContentType, "application/tar")]
      , requestBody    = RequestBodyLBS tar
      }

    responseBody response $$+- CL.map showStream
                          =$=  CB.sinkHandle stdout

    imageId <$> getImage latest
  where
    tar = tarDockerfile dockerfile

    -- TODO This should probably be a unique repo/tag
    -- TODO that we delete after building.
    latest = RepoTag "latest" "latest"
    query = "?t=" <> encodeRepoTag latest

------------------------------------------------------------------------

create :: Identifier a
       => Maybe ContainerName
       -> CreateContainer a
       -> Docker CreateContainerResponse
create name cc = responseBody =<< dockerRequest (uri <> query) go
  where
    uri   = "/containers/create"
    query = maybe "" ("?name=" <>) name
    go x  = x { method         = "POST"
              , requestHeaders = [(hContentType, "application/json")]
              , requestBody    = RequestBodyLBS (A.encode cc) }

containerFrom :: Identifier a => a -> CreateContainer a
containerFrom image = CreateContainer {
      createImage        = image
    , createHostName     = ""
    , createDomainName   = ""
    , createCommand      = []
    , createExposedPorts = S.empty
    , createPortBindings = M.empty
    , createHostConfig   = HostConfig {
        hostDns       = []
      , hostDnsSearch = []
      }
    }

------------------------------------------------------------------------

start :: Id -> Docker ()
start i = responseBody <$> dockerRequest_ uri go
  where
    uri  = "/containers/" <> encodeIdentifier i <> "/start"
    go x = x { method         = "POST"
             , requestHeaders = [(hContentType, "application/json")]
             , requestBody    = RequestBodyLBS (A.encode (A.object [])) }

stop :: Id -> TimeoutSeconds -> Docker ()
stop i timeout = responseBody <$> dockerRequest_ (uri <> query) go
  where
    uri   = "/containers/" <> encodeIdentifier i <> "/stop"
    query = "?t=" <> T.pack (show timeout)
    go x  = x { method = "POST" }

------------------------------------------------------------------------
-- Streaming Status

data Stream = Stream        Text
            | StreamError   Error
            | StreamStatus  Status
            | StreamUnknown A.Value
  deriving (Eq, Show)

data Status = Status {
      _statusSubject      :: Text
    , _statusId           :: Maybe Text
    , _statusProgress     :: Maybe Progress
    , _statusProgressText :: Maybe Text
    } deriving (Eq, Ord, Show)

data Progress = Progress {
      _progressCurrent :: Int64
    , _progressTotal   :: Int64
    } deriving (Eq, Ord, Show)

data Error = Error {
      _errorSubject :: Text
    , _errorCode    :: Maybe Int
    , _errorDetail  :: Text
    } deriving (Eq, Ord, Show)

------------------------------------------------------------------------
-- Images

data Image = Image {
    imageId          :: Id
  , imageParent      :: Maybe Id
  , imageSize        :: Int64
  , imageVirtualSize :: Int64
  , imageRepoTags    :: [RepoTag]
  } deriving (Eq, Ord, Show)

------------------------------------------------------------------------
-- Containers

type HostName    = Text
type DomainName  = Text
type HostAddress = IPAddress
type IPPrefixLen = Int

newtype IPAddress = IPAddress { unIP :: Text }
    deriving (Eq, Ord)

newtype MacAddress = MacAddress { unMAC :: Text }
    deriving (Eq, Ord)

data Protocol = TCP | UDP
    deriving (Eq, Ord, Show)

data SrcPort = SrcPort Word16 Protocol
    deriving (Eq, Ord, Show)

data DstPort = DstPort Word16 (Maybe HostAddress)
    deriving (Eq, Ord, Show)

type TimeoutSeconds = Int

data CreateContainer a = CreateContainer {
      createImage        :: a
    , createHostName     :: HostName
    , createDomainName   :: DomainName
    , createCommand      :: [Text]
    , createExposedPorts :: Set SrcPort
    , createPortBindings :: Map SrcPort [DstPort]
    , createHostConfig   :: HostConfig
    } deriving (Eq, Ord, Show)

data CreateContainerResponse = CreateContainerResponse {
      createId       :: Id
    , createWarnings :: [Text]
    } deriving (Eq, Ord, Show)

data Container = Container {
      containerId         :: Id
    , containerImage      :: Id
    , containerName       :: ContainerName
    , containerPath       :: FilePath
    , containerArgs       :: [Text]
    , containerCreated    :: UTCTime
    , containerNetwork    :: NetworkSettings
    , containerHostConfig :: HostConfig
    } deriving (Eq, Ord, Show)

data HostConfig = HostConfig {
      hostDns       :: [IPAddress]
    , hostDnsSearch :: [DomainName]
    } deriving (Eq, Ord, Show)

data NetworkSettings = NetworkSettings {
      networkGateway     :: IPAddress
    , networkIPAddress   :: IPAddress
    , networkIPPrefixLen :: IPPrefixLen
    , networkMacAddress  :: MacAddress
    } deriving (Eq, Ord, Show)

------------------------------------------------------------------------

instance Identifier a => A.ToJSON (CreateContainer a) where
    toJSON CreateContainer{..} = A.object [
          "Image"        .= encodeIdentifier createImage
        , "Hostname"     .= createHostName
        , "Domainname"   .= createDomainName
        , "Cmd"          .= createCommand
        , "ExposedPorts" .= createExposedPorts
        , "PortBindings" .= createPortBindings
        , "HostConfig"   .= createHostConfig
        ]

instance A.ToJSON HostConfig where
    toJSON HostConfig{..} = A.object [
          "Dns"       .= hostDns
        , "DnsSearch" .= hostDnsSearch
        ]

instance A.ToJSON (Set SrcPort) where
    toJSON = A.object . map toPair . S.toList
      where
        toPair src = encodeSrcPort src .= A.object []

instance A.ToJSON (Map SrcPort [DstPort]) where
    toJSON = A.object . map toPair . M.toList
      where
        toPair (src, dsts) = encodeSrcPort src .= dsts

instance A.ToJSON DstPort where
    toJSON (DstPort port addr) = A.object [
          "HostPort"    .= show port
        , "HostAddress" .= maybe "0.0.0.0" unIP addr
        ]

instance A.ToJSON IPAddress where
    toJSON = A.String . unIP

encodeProtocol :: Protocol -> Text
encodeProtocol TCP = "tcp"
encodeProtocol UDP = "udp"

encodeSrcPort :: SrcPort -> Text
encodeSrcPort (SrcPort port proto) = T.pack (show port) <> "/" <> encodeProtocol proto

------------------------------------------------------------------------

instance A.FromJSON Id where
    parseJSON = A.withText "Id" (pure . Id . T.encodeUtf8)

instance A.FromJSON Image where
    parseJSON = A.withObject "Image" $ \o -> do
                Image <$> (o .: "Id")
                      <*> (decodeMaybeId <$> o .:: ["Parent", "ParentId"])
                      <*> (o .: "Size")
                      <*> (o .: "VirtualSize")
                      <*> (mapMaybe decodeRepoTag . fromMaybe [] <$> (o .:? "RepoTags"))

instance A.FromJSON Stream where
    parseJSON v = A.withObject "Stream" (\o -> Stream <$> (o .: "stream")) v
              <|> StreamError  <$> A.parseJSON v
              <|> StreamStatus <$> A.parseJSON v
              <|> pure (StreamUnknown v)

instance A.FromJSON Status where
    parseJSON = A.withObject "Status" $ \o -> do
                subject      <- o .:  "status"
                statusId     <- o .:? "id"
                progressText <- o .:? "progress"
                mprogress    <- o .:? "progressDetail"

                progress <- case mprogress of
                    Nothing -> pure Nothing
                    Just p  -> do
                        current <- p .:? "current"
                        total   <- p .:? "total"
                        return (Progress <$> current <*> total)

                return (Status subject statusId progress progressText)

-- { "status": "Extracting"
-- , "progress": "[============================>                      ] 53.48 MB/92.46 MB"
-- , "progressDetail": { "current": 53477376, "total": 92463446 }
-- , "id": "e26efd418c48" }

instance A.FromJSON Error where
    parseJSON = A.withObject "Error" $ \o -> do
                subject <- o .: "error"
                detail  <- o .: "errorDetail"
                code    <- detail .:? "code"
                message <- detail .:  "message"
                return (Error subject code message)

instance A.FromJSON CreateContainerResponse where
    parseJSON = A.withObject "CreateContainerResponse" $ \o -> do
                CreateContainerResponse <$> o .: "Id"
                                        <*> (fromMaybe [] <$> o .: "Warnings")

instance A.FromJSON Container where
    parseJSON = A.withObject "Container" $ \o -> do
                Container <$> o .: "Id"
                          <*> o .: "Image"
                          <*> o .: "Name"
                          <*> o .: "Path"
                          <*> o .: "Args"
                          <*> o .: "Created"
                          <*> o .: "NetworkSettings"
                          <*> o .: "HostConfig"

instance A.FromJSON NetworkSettings where
    parseJSON = A.withObject "NetworkSettings" $ \o -> do
                NetworkSettings <$> o .: "Gateway"
                                <*> o .: "IPAddress"
                                <*> o .: "IPPrefixLen"
                                <*> o .: "MacAddress"

instance A.FromJSON HostConfig where
    parseJSON = A.withObject "HostConfig" $ \o -> do
                HostConfig <$> (fromMaybe [] <$> o .: "Dns")
                           <*> (fromMaybe [] <$> o .: "DnsSearch")

instance A.FromJSON IPAddress where
    parseJSON = A.withText "IPAddress" (pure . IPAddress)

instance A.FromJSON MacAddress where
    parseJSON = A.withText "MacAddress" (pure . MacAddress)

------------------------------------------------------------------------

showStream :: Stream -> B.ByteString
showStream (Stream        txt) = T.encodeUtf8 txt
showStream (StreamError   err) = T.encodeUtf8 (T.pack (show err)) <> "\n"
showStream (StreamStatus  sts) = T.encodeUtf8 (T.pack (show sts)) <> "\n"
showStream (StreamUnknown unk) = L.toStrict (A.encode unk)        <> "\n"

-- Yay, Docker calls some fields different things depending on which route you
-- call (e.g. Parent vs ParentId).
(.::) :: A.FromJSON a => A.Object -> [Text] -> A.Parser a
(.::) _ []     = empty
(.::) o (x:xs) = (o .: x) <|> (o .:: xs)

decodeMaybeId :: Text -> Maybe Id
decodeMaybeId txt | T.null txt = Nothing
                  | otherwise  = Just (Id (T.encodeUtf8 txt))

------------------------------------------------------------------------
-- Custom Show Instances

instance Show Id where
  showsPrec p (Id x) =
    showParen (p > 10) $ showString "Id "
                       . showsPrec 11 x

instance Show IPAddress where
  showsPrec p (IPAddress x) =
    showParen (p > 10) $ showString "IPAddress "
                       . showsPrec 11 x

instance Show MacAddress where
  showsPrec p (MacAddress x) =
    showParen (p > 10) $ showString "MacAddress "
                       . showsPrec 11 x
