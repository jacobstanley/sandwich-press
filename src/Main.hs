{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.IO.Class (liftIO)
import Data.Monoid ((<>))

import Network.Docker

------------------------------------------------------------------------

main :: IO ()
main = runDocker $ do
    id0 <- build (dnsMasq    :: Dockerfile "centos" "6")
    id1 <- build (oracleJDK7 :: Dockerfile "centos" "6")
    id2 <- build (oracleJDK7 :: Dockerfile "centos" "7")
    liftIO (mapM_ print [id0, id1, id2])
    xs <- getImages
    liftIO (mapM_ print xs)

------------------------------------------------------------------------

dnsMasq :: Dockerfile "centos" tag
dnsMasq = run ["yum", "install", "-y", "dnsmasq"]

------------------------------------------------------------------------

wget :: Dockerfile "centos" tag
wget = run ["yum", "install", "-y", "wget"]

------------------------------------------------------------------------

oracleJDK7 :: Dockerfile "centos" tag
oracleJDK7 = wget <> runMany [fetch, install, rm]
  where
    fetch = [ "wget", "--no-cookies"
                    , "--progress=bar:force"
                    , "--header", "Cookie: oraclelicense=accept-securebackup-cookie"
                    , "http://download.oracle.com/otn-pub/java/jdk/7u76-b13/jdk-7u76-linux-x64.rpm"
                    , "-O", "jdk7.rpm"]

    install = [ "yum", "install", "-y", "jdk7.rpm" ]
    rm      = [ "rm", "jdk7.rpm" ]
