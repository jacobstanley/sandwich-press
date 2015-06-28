{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Docker.Redhat (
      Redhat
    , Redhat6
    , Redhat7
    , centos6
    , centos7

    , Wget
    , wget
    ) where

import Data.Dockerfile

------------------------------------------------------------------------

data Redhat
data Redhat6
data Redhat7

centos6 :: Dockerfile '[Redhat, Redhat6]
centos6 = base (RepoTag "centos" "6")

centos7 :: Dockerfile '[Redhat, Redhat7]
centos7 = base (RepoTag "centos" "7")

------------------------------------------------------------------------

data Wget

wget :: Requires Redhat cs => Fragment cs (Wget ': cs)
wget = run ["yum", "install", "-y", "wget"]
   >>> addCapability
