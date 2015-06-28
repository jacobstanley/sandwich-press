{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Data.Dockerfile (
      RepoTag(..)
    , Dockerfile
    , Fragment
    , Capability(..)

    , FilePath
    , FileMode

    , addCapability
    , from
    , base

    , copy
    , copyUtf8
    , run
    , runMany
    , cmd
    , cmdMany
    , entrypoint

    , encodeDockerfile
    , encodeRepoTag
    , decodeRepoTag

    , tarDockerfile

    -- * Re-exported from 'Text.Shakespeare.Text'
    , lt
    , lbt

    -- * Re-exported from 'Control.Category'
    , (<<<)
    , (>>>)

    -- * Re-export 'Data.Dockerfile.TypeLevel'
    , module Data.Dockerfile.TypeLevel
    ) where

import qualified Codec.Archive.Tar as Tar
import qualified Codec.Archive.Tar.Entry as Tar
import           Control.Category (Category(..), (<<<), (>>>))
import qualified Data.ByteString.Lazy as L
import           Data.List (intercalate)
import           Data.Maybe (mapMaybe)
import           Data.Monoid ((<>))
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LT
import           Prelude hiding ((.))
import           System.Posix.Types (FileMode)
import           Text.Shakespeare.Text (lt, lbt)

import           Data.Dockerfile.TypeLevel

------------------------------------------------------------------------

data RepoTag = RepoTag {
    repoName :: Text
  , tagName  :: Text
  } deriving (Eq, Ord)

data Capability c = Capability

data Dockerfile (capabilities :: [*]) = Dockerfile {
    baseImage    :: RepoTag
  , instructions :: [Instruction]
  } deriving (Eq, Ord, Show)

newtype Fragment (requires :: [*]) (provides :: [*]) = Fragment [Instruction]
  deriving (Eq, Ord, Show)

data Instruction =
    COPY       FileMode FilePath L.ByteString
  | RUN        [Text]
  | CMD        [Text]
  | ENTRYPOINT [Text]
  deriving (Eq, Ord, Show)

------------------------------------------------------------------------

addCapability :: Fragment cs (c ': cs)
addCapability = Fragment []

liftI :: Instruction -> Fragment cs cs
liftI i = Fragment [i]

instance Category Fragment where
    id                                = Fragment []
    (.) (Fragment b2c) (Fragment a2b) = Fragment (a2b ++ b2c)

------------------------------------------------------------------------

from :: Dockerfile cs -> Fragment cs ds -> Dockerfile ds
from (Dockerfile rt xs) (Fragment ys) = Dockerfile rt (xs ++ ys)

base :: RepoTag -> Dockerfile cs
base rt = Dockerfile rt []

------------------------------------------------------------------------

copy :: FileMode -> FilePath -> L.ByteString -> Fragment cs cs
copy mode path bs = liftI (COPY mode path bs)

copyUtf8 :: FileMode -> FilePath -> LT.Text -> Fragment cs cs
copyUtf8 mode path txt = liftI (COPY mode path (LT.encodeUtf8 txt))

------------------------------------------------------------------------

run :: [Text] -> Fragment cs cs
run xs = liftI (RUN xs)

runMany :: [[Text]] -> Fragment cs cs
runMany xss = run (shellMany xss)

------------------------------------------------------------------------

cmd :: [Text] -> Fragment cs cs
cmd xs = liftI (CMD xs)

cmdMany :: [[Text]] -> Fragment cs cs
cmdMany xss = cmd (shellMany xss)

------------------------------------------------------------------------

entrypoint :: [Text] -> Fragment cs cs
entrypoint xs = liftI (ENTRYPOINT xs)

------------------------------------------------------------------------

encodeDockerfile :: Dockerfile cs -> Text
encodeDockerfile df =
    T.unlines $ [ "FROM " <> encodeRepoTag (baseImage df) ]
             <> map encodeInstruction (instructions df)

encodeInstruction :: Instruction -> Text
encodeInstruction i = case i of
    COPY     _ path _ -> "COPY " <> T.dropWhile (== '/') (T.pack path)
                                 <> " "
                                 <> T.pack path
    RUN        args   -> "RUN "        <> quotedList args
    CMD        args   -> "CMD "        <> quotedList args
    ENTRYPOINT args   -> "ENTRYPOINT " <> quotedList args

------------------------------------------------------------------------

encodeRepoTag :: RepoTag -> Text
encodeRepoTag (RepoTag repo tag) = repo <> ":" <> tag

decodeRepoTag :: Text -> Maybe RepoTag
decodeRepoTag txt
    | txt == "<none>:<none>" = Nothing
    | otherwise              = Just (RepoTag repo (T.drop 1 tag))
  where
    (repo, tag) = T.break (== ':') txt

------------------------------------------------------------------------

tarDockerfile :: Dockerfile cs -> L.ByteString
tarDockerfile df = mkTar $ [(0o444, "Dockerfile", dockerfile)] ++ files
  where
    dockerfile = L.fromStrict
               . T.encodeUtf8
               $ encodeDockerfile df

    files = mapMaybe file (instructions df)

    file (COPY mode path bs) = Just (mode, path, bs)
    file _                   = Nothing

------------------------------------------------------------------------

mkTar :: [(FileMode, FilePath, L.ByteString)] -> L.ByteString
mkTar = Tar.write . map go
  where
    go (mode, file, bs) = (Tar.fileEntry (fromPath file) bs)
                          { Tar.entryPermissions = mode }

    fromPath file = case Tar.toTarPath False file of
      Left msg -> error ("mkTar: " ++ msg)
      Right x  -> x

------------------------------------------------------------------------

shellMany :: [[Text]] -> [Text]
shellMany xss = ["sh", "-c", ys]
  where
    ys = T.unwords
       . intercalate ["&&"]
       . map (map quote')
       $ xss

quotedList :: [Text] -> Text
quotedList xs = "[" <> T.intercalate ", " (map quote xs) <> "]"

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
                || x == ';'
                || x == '\f'
                || x == '\n'
                || x == '\r'
                || x == '\t'
                || x == '\v'

------------------------------------------------------------------------
-- Custom Show Instances

instance Show RepoTag where
  showsPrec p (RepoTag r t) =
    showParen (p > 10) $ showString "RepoTag "
                       . showsPrec 11 r
                       . showString " "
                       . showsPrec 11 t
