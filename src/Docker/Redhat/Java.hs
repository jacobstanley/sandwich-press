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
           => Requires Wget   cs
           => Fragment cs (Java ': Java7 ': cs)
oracleJDK7 = runMany [fetch, install, rm]
         >>> addCapability
         >>> addCapability
  where
    fetch = [ "wget", "--no-cookies"
                    , "--progress=bar:force"
                    , "--header", "Cookie: oraclelicense=accept-securebackup-cookie"
                    , "http://download.oracle.com/otn-pub/java/jdk/7u76-b13/jdk-7u76-linux-x64.rpm"
                    , "-O", "jdk7.rpm" ]

    install = [ "yum", "install", "-y", "jdk7.rpm" ]
    rm      = [ "rm", "jdk7.rpm" ]
