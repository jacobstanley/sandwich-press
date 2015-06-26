{-# LANGUAGE OverloadedStrings #-}

module Docker.Redhat.Java (
      oracleJDK7
    ) where

import Data.Dockerfile

import Docker.Redhat

------------------------------------------------------------------------

oracleJDK7 :: Redhat repo => Dockerfile repo tag
oracleJDK7 = wget <> runMany [fetch, install, rm]
  where
    fetch = [ "wget", "--no-cookies"
                    , "--progress=bar:force"
                    , "--header", "Cookie: oraclelicense=accept-securebackup-cookie"
                    , "http://download.oracle.com/otn-pub/java/jdk/7u76-b13/jdk-7u76-linux-x64.rpm"
                    , "-O", "jdk7.rpm" ]

    install = [ "yum", "install", "-y", "jdk7.rpm" ]
    rm      = [ "rm", "jdk7.rpm" ]
