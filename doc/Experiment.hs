{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -w #-}

module Data.Fragment where

import           Control.Category (Category(..), (<<<), (>>>))
import qualified Data.ByteString.Lazy as L
import           Data.Text (Text)
import           GHC.Exts (Constraint)
import           GHC.TypeLits
import           Prelude hiding (id, (.))
import           System.Posix.Types (FileMode)

------------------------------------------------------------------------

type Repo = Text
type Tag  = Text

data Dockerfile (capabilities :: [*]) = Dockerfile Repo Tag [Instruction]
  deriving (Eq, Ord, Show)

newtype Fragment (requires :: [*]) (provides :: [*]) = Fragment [Instruction]
  deriving (Eq, Ord, Show)

data Instruction =
    COPY       FileMode FilePath L.ByteString
  | RUN        [Text]
  | CMD        [Text]
  | ENTRYPOINT [Text]
  deriving (Eq, Ord, Show)

data Capability c = Capability

------------------------------------------------------------------------

provides :: Capability c -> Fragment cs (c ': cs)
provides Capability = Fragment []

liftInstruction :: Instruction -> Fragment cs cs
liftInstruction i = Fragment [i]

instance Category Fragment where
    id                                = Fragment []
    (.) (Fragment b2c) (Fragment a2b) = Fragment (a2b ++ b2c)

from :: Dockerfile cs -> Fragment cs ds -> Dockerfile ds
from (Dockerfile repo tag xs) (Fragment ys) = Dockerfile repo tag (xs ++ ys)

------------------------------------------------------------------------

type family Or (x :: Bool) (y :: Bool) :: Bool where
  Or False False = False
  Or _x    _y    = True

type family Contains (needle :: *) (haystack :: [*]) :: Bool where
  Contains x '[]       = False
  Contains x (x ': ys) = True
  Contains x (y ': ys) = Contains x ys

type Requires x cs = Contains x cs ~ True

type family ContainsOneOf (needles :: [*]) (haystack :: [*]) :: Bool where
  ContainsOneOf '[]       ys = False
  ContainsOneOf (x ': xs) ys = Contains x ys `Or` ContainsOneOf xs ys

type RequiresOneOf xs cs = ContainsOneOf xs cs ~ True

------------------------------------------------------------------------

run :: [Text] -> Fragment cs cs
run args = liftInstruction (RUN args)

------------------------------------------------------------------------

data Redhat

data Debian

centos6 :: Dockerfile '[Redhat]
centos6 = Dockerfile "centos" "6" []

debian8 :: Dockerfile '[Debian]
debian8 = Dockerfile "debian" "jessie" []

------------------------------------------------------------------------

data Wget

wget :: Requires Redhat cs => Fragment cs (Wget ': cs)
wget = provides (Capability :: Capability Wget)
   >>> run ["yum", "install", "-y", "wget"]

dwget :: Requires Debian cs => Fragment cs (Wget ': cs)
dwget = provides (Capability :: Capability Wget)
    >>> run ["apt-get", "install", "-y", "wget"]

------------------------------------------------------------------------

example1 :: Dockerfile '[Wget, Redhat]
example1 = from centos6 wget

------------------------------------------------------------------------

-- | Append for type-level lists.
type family (as :: [k]) ++ (bs :: [k]) :: [k] where
  '[]       ++ bs = bs
  (a ': as) ++ bs = a ': (as ++ bs)
