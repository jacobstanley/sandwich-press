{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Dockerfile (
      RepoTag(..)
    , Dockerfile

    , FilePath
    , FileMode

    , (<>)

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

    -- * Re-exported from 'GHC.TypeLits'
    , Symbol

    -- * Re-exported from 'Text.Shakespeare.Text'
    , lt
    , lbt
    ) where

import qualified Codec.Archive.Tar as Tar
import qualified Codec.Archive.Tar.Entry as Tar
import qualified Data.ByteString.Lazy as L
import           Data.List (intercalate)
import           Data.Maybe (mapMaybe)
import           Data.Monoid ((<>))
import           Data.Proxy (Proxy(..))
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LT
import           GHC.TypeLits (Symbol, KnownSymbol, symbolVal)
import           System.Posix.Types (FileMode)
import           Text.Shakespeare.Text (lt, lbt)

------------------------------------------------------------------------

data RepoTag = RepoTag {
    repoName :: Text
  , tagName  :: Text
  } deriving (Eq, Ord)

type Dockerfile (repo :: Symbol) (tag :: Symbol)
    = [Instruction repo tag]

data Instruction (repo :: Symbol) (tag :: Symbol) where
    COPY       :: FileMode -> FilePath -> L.ByteString -> Instruction repo tag
    RUN        :: [Text]                               -> Instruction repo tag
    CMD        :: [Text]                               -> Instruction repo tag
    ENTRYPOINT :: [Text]                               -> Instruction repo tag

------------------------------------------------------------------------

copy :: FileMode -> FilePath -> L.ByteString -> Dockerfile repo tag
copy mode path bs = [COPY mode path bs]

copyUtf8 :: FileMode -> FilePath -> LT.Text -> Dockerfile repo tag
copyUtf8 mode path txt = [COPY mode path (LT.encodeUtf8 txt)]

------------------------------------------------------------------------

run :: [Text] -> Dockerfile repo tag
run xs = [RUN xs]

runMany :: [[Text]] -> Dockerfile repo tag
runMany xss = run (shellMany xss)

------------------------------------------------------------------------

cmd :: [Text] -> Dockerfile repo tag
cmd xs = [CMD xs]

cmdMany :: [[Text]] -> Dockerfile repo tag
cmdMany xss = cmd (shellMany xss)

------------------------------------------------------------------------

entrypoint :: [Text] -> Dockerfile repo tag
entrypoint xs = [ENTRYPOINT xs]

------------------------------------------------------------------------

encodeDockerfile :: (KnownSymbol repo, KnownSymbol tag) => Dockerfile repo tag -> Text
encodeDockerfile xs = T.unlines $
    [ "FROM " <> encodeRepoTag (baseImage xs) ] <> map encodeInstruction xs

encodeInstruction :: Instruction repo tag -> Text
encodeInstruction i = case i of
    COPY     _ path _ -> "COPY " <> T.dropWhile (== '/') (T.pack path)
                                 <> " "
                                 <> T.pack path
    RUN        args   -> "RUN "        <> quotedList args
    CMD        args   -> "CMD "        <> quotedList args
    ENTRYPOINT args   -> "ENTRYPOINT " <> quotedList args

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

------------------------------------------------------------------------

tarDockerfile :: (KnownSymbol repo, KnownSymbol tag) => Dockerfile repo tag -> L.ByteString
tarDockerfile xs = mkTar $ [(0o444, "Dockerfile", dockerfile)] ++ files
  where
    dockerfile = L.fromStrict
               . T.encodeUtf8
               $ encodeDockerfile xs

    files = mapMaybe file xs

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
