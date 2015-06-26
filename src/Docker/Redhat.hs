{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}

module Docker.Redhat (
      Redhat
    , wget
    ) where

import Data.Dockerfile

------------------------------------------------------------------------

class Redhat (repo :: Symbol)

instance Redhat "centos"

------------------------------------------------------------------------

wget :: Redhat repo => Dockerfile repo tag
wget = run ["yum", "install", "-y", "wget"]
