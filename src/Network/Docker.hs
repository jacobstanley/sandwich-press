{-# LANGUAGE OverloadedStrings #-}

module Network.Docker (
      build
    ) where

import qualified Codec.Archive.Tar as Tar
import qualified Codec.Archive.Tar.Entry as Tar
import           Data.Aeson ((.:))
import qualified Data.Aeson as A
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import           Data.Conduit ((=$=), ($$+-))
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.List as CL
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Network.HTTP.Conduit (Request(..), RequestBody(..), Response(..))
import           Network.HTTP.Types.Header (hContentType)
import           System.IO (stdout)

import           Network.Docker.Internal

------------------------------------------------------------------------

build :: IO ()
build = runDocker $ do
    let tar = mkTar [("Dockerfile", "FROM centos:6\nMAINTAINER xxx\nRUN yum install -y wget\n")]

    response <- dockerRequest "/build" $ \x -> x {
        method         = "POST"
      , requestHeaders = [(hContentType, "application/tar")]
      , requestBody    = RequestBodyLBS tar
      }

    responseBody response $$+- CL.map showStream
                          =$=  CB.sinkHandle stdout

------------------------------------------------------------------------

data Stream = Stream T.Text

instance A.FromJSON Stream where
    parseJSON = "stream object" `A.withObject`
                \o -> Stream <$> (o .: "stream")


showStream :: Stream -> B.ByteString
showStream (Stream txt) = T.encodeUtf8 txt

------------------------------------------------------------------------

mkTar :: [(FilePath, L.ByteString)] -> L.ByteString
mkTar = Tar.write . map go
  where
    go (file, bs) = Tar.fileEntry (fromPath file) bs

    fromPath file = case Tar.toTarPath False file of
      Left msg -> error ("mkTar: " ++ msg)
      Right x  -> x
