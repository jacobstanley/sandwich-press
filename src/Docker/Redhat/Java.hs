{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Docker.Redhat.Java (
      Java
    , Java7
    , oracleJDK7
    ) where

import Data.Dockerfile

import Docker.Redhat

------------------------------------------------------------------------

data Java
data Java7

oracleJDK7 :: Requires Redhat cs
           => Fragment cs (Java ': Java7 ': cs)
oracleJDK7 = installRpm' [("oraclelicense", "accept-securebackup-cookie")] url sha
         >>> addCapability
         >>> addCapability
  where
    url = "http://download.oracle.com/otn-pub/java/jdk/7u80-b15/jdk-7u80-linux-x64.rpm"
    sha = SHA256 "405d5fb7fa8cc4b5e5fe2c5fa349af6fbd742d9967772163d1fa8ea4ce35cd7b"
