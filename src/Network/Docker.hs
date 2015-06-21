{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Network.Docker (
      Id(..)
    , RepoTag(..)
    , Image(..)

    , Dockerfile
    , Instruction(..)

    , run
    , runMany

    , Docker
    , DockerT
    , runDocker

    , build
    , getImages
    , getImage

    , encodeDockerfile
    , encodeInstruction
    ) where

import qualified Codec.Archive.Tar as Tar
import qualified Codec.Archive.Tar.Entry as Tar
import           Control.Applicative ((<|>), empty)
import           Data.Aeson ((.:), (.:?))
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import           Data.Conduit ((=$=), ($$+-))
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.List as CL
import           Data.Int (Int64)
import           Data.List (intercalate)
import           Data.Maybe (fromMaybe, mapMaybe)
import           Data.Monoid ((<>))
import           Data.Proxy (Proxy(..))
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           GHC.TypeLits
import           Network.HTTP.Conduit (Request(..), RequestBody(..), Response(..))
import           Network.HTTP.Types.Header (hContentType)
import           System.IO (stdout)

import           Network.Docker.Internal

------------------------------------------------------------------------

newtype Id = Id { unId :: B.ByteString }
  deriving (Eq, Ord)

data RepoTag = RepoTag {
    repoName :: Text
  , tagName  :: Text
  } deriving (Eq, Ord)

instance Show Id where
  showsPrec p (Id x) =
    showParen (p > 10) $ showString "Id "
                       . showsPrec 11 x

instance Show RepoTag where
  showsPrec p (RepoTag r t) =
    showParen (p > 10) $ showString "RepoTag "
                       . showsPrec 11 r
                       . showString " "
                       . showsPrec 11 t

------------------------------------------------------------------------

type Dockerfile (repo :: Symbol) (tag :: Symbol)
    = [Instruction repo tag]

data Instruction (repo :: Symbol) (tag :: Symbol) where
    RUN :: [Text] -> Instruction repo tag

------------------------------------------------------------------------

run :: [Text] -> Dockerfile repo tag
run xs = [RUN xs]

runMany :: [[Text]] -> Dockerfile repo tag
runMany xss = run $ ["sh", "-c", ys]
  where
    ys = T.unwords
       . intercalate ["&&"]
       . map (map quote')
       $ xss

------------------------------------------------------------------------

baseImage :: forall repo tag.  (KnownSymbol repo, KnownSymbol tag)
            => Dockerfile repo tag -> RepoTag
baseImage _ = RepoTag (T.pack repo) (T.pack tag)
  where
    repo = symbolVal (Proxy :: Proxy repo)
    tag  = symbolVal (Proxy :: Proxy tag)

encodeRepoTag :: RepoTag -> Text
encodeRepoTag (RepoTag repo tag) = repo <> ":" <> tag

decodeRepoTag :: Text -> Maybe RepoTag
decodeRepoTag txt
    | txt == "<none>:<none>" = Nothing
    | otherwise              = Just (RepoTag repo (T.drop 1 tag))
  where
    (repo, tag) = T.break (== ':') txt

encodeDockerfile :: (KnownSymbol repo, KnownSymbol tag) => Dockerfile repo tag -> Text
encodeDockerfile xs = T.unlines $
    [ "FROM " <> encodeRepoTag (baseImage xs) ] <> map encodeInstruction xs

encodeInstruction :: Instruction repo tag -> Text
encodeInstruction (RUN args) =
    "RUN [" <> T.intercalate ", " quoted <> "]"
  where
    quoted = map quote args

quote :: Text -> Text
quote xs = "\"" <> escape xs <> "\""
  where
    escape = T.replace "\"" "\\\""
           . T.replace "\\" "\\\\"
           . T.replace "\f" "\\f"
           . T.replace "\n" "\\n"
           . T.replace "\r" "\\r"
           . T.replace "\t" "\\t"
           . T.replace "\v" "\\v"

quote' :: Text -> Text
quote' xs | T.any needsQuote xs = quote xs
          | otherwise           = xs
  where
    needsQuote x = x == '\\'
                || x == '\"'
                || x == ' '
                || x == '\f'
                || x == '\n'
                || x == '\r'
                || x == '\t'
                || x == '\v'

------------------------------------------------------------------------

build :: (KnownSymbol repo, KnownSymbol tag) => Dockerfile repo tag -> Docker Id
build xs = do
    response0 <- dockerRequestStream ("/build" <> query) $ \x -> x {
        method         = "POST"
      , requestHeaders = [(hContentType, "application/tar")]
      , requestBody    = RequestBodyLBS tar
      }

    responseBody response0 $$+- CL.map showStream
                           =$=  CB.sinkHandle stdout

    imageId <$> getImage latest
  where
    tar = mkTar [("Dockerfile", dockerfile)]

    latest = RepoTag "latest" "latest"
    query = "?t=" <> encodeRepoTag latest

    dockerfile = L.fromStrict
               . T.encodeUtf8
               $ encodeDockerfile xs

------------------------------------------------------------------------

getImages :: Docker [Image]
getImages = responseBody =<< dockerRequest "/images/json?all=true" id

getImage :: RepoTag -> Docker Image
getImage repoTag = responseBody =<< dockerRequest uri id
  where
    uri = "/images/" <> encodeRepoTag repoTag <> "/json"

------------------------------------------------------------------------

data Image = Image {
    imageId          :: Id
  , imageParent      :: Maybe Id
  , imageSize        :: Int64
  , imageVirtualSize :: Int64
  , imageRepoTags    :: [RepoTag]
  } deriving (Eq, Ord, Show)

data Stream = Stream Text
            | StreamError Error
  deriving (Eq, Ord, Show)

data Error = Error {
      _errorSubject :: Text
    , _errorCode    :: Int
    , _errorDetail  :: Text
    } deriving (Eq, Ord, Show)

------------------------------------------------------------------------

instance A.FromJSON Id where
    parseJSON = A.withText "Id" (pure . Id . T.encodeUtf8)

instance A.FromJSON Image where
    parseJSON = A.withObject "Image" $ \o -> do
                Image <$> (o .: "Id")
                      <*> (o .: "Size")
                      <*> (o .: "VirtualSize")
                      <*> (decodeMaybeId <$> o .:: ["Parent", "ParentId"])
                      <*> (mapMaybe decodeRepoTag . fromMaybe [] <$> (o .:? "RepoTags"))

instance A.FromJSON Stream where
    parseJSON v = A.withObject "Stream" (\o -> Stream <$> (o .: "stream")) v
            <|> StreamError <$> A.parseJSON v

instance A.FromJSON Error where
    parseJSON = A.withObject "Error" $ \o -> do
                subject <- o .: "error"
                detail  <- o .: "errorDetail"
                code    <- detail .: "code"
                message <- detail .: "message"
                return (Error subject code message)


showStream :: Stream -> B.ByteString
showStream (Stream      txt) = T.encodeUtf8 txt
showStream (StreamError err) = T.encodeUtf8 (T.pack (show err))

-- Yay, Docker calls some fields different things depending on which route you
-- call (e.g. Parent vs ParentId).
(.::) :: A.FromJSON a => A.Object -> [Text] -> A.Parser a
(.::) _ []     = empty
(.::) o (x:xs) = (o .: x) <|> (o .:: xs)

decodeMaybeId :: Text -> Maybe Id
decodeMaybeId txt | T.null txt = Nothing
                  | otherwise  = Just (Id (T.encodeUtf8 txt))

------------------------------------------------------------------------

mkTar :: [(FilePath, L.ByteString)] -> L.ByteString
mkTar = Tar.write . map go
  where
    go (file, bs) = Tar.fileEntry (fromPath file) bs

    fromPath file = case Tar.toTarPath False file of
      Left msg -> error ("mkTar: " ++ msg)
      Right x  -> x
