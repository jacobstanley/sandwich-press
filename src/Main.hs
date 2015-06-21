{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

--import           Control.Monad.IO.Class (liftIO)

import           Data.Monoid ((<>))
import           GHC.TypeLits (Symbol)

import qualified Docker.Consul as Consul
import           Network.Docker

------------------------------------------------------------------------

main :: IO ()
main = runDocker $ do
    --liftIO . print =<< getImage (RepoTag "centos" "6")
    --liftIO . print =<< getImage (Id "31cecc9f6186")

    _ <- Consul.startCluster 3

    --liftIO (threadDelay 100000000)
    --stop (createId consul) 5

    --id0 <- build (dnsMasq    :: Dockerfile "centos" "6")
    --id1 <- build (oracleJDK7 :: Dockerfile "centos" "6")
    --id2 <- build (oracleJDK7 :: Dockerfile "centos" "7")
    --id3 <- build (oracleJDK7 :: Dockerfile "fedora" "21")
    --id4 <- build (oracleJDK7 :: Dockerfile "fedora" "22")
    --liftIO (mapM_ print [id0, id1, id2, id3, id4])

    return ()

------------------------------------------------------------------------

class Yum (repo :: Symbol)

instance Yum "centos"
instance Yum "fedora"

class Apt (repo :: Symbol)

instance Apt "ubuntu"
instance Apt "debian"

------------------------------------------------------------------------

dnsMasq :: Yum repo => Dockerfile repo tag
dnsMasq = run ["yum", "install", "-y", "dnsmasq"]

------------------------------------------------------------------------

wget :: Yum repo => Dockerfile repo tag
wget = run ["yum", "install", "-y", "wget"]

------------------------------------------------------------------------

oracleJDK7 :: Yum repo => Dockerfile repo tag
oracleJDK7 = wget <> runMany [fetch, install, rm]
  where
    fetch = [ "wget", "--no-cookies"
                    , "--progress=bar:force"
                    , "--header", "Cookie: oraclelicense=accept-securebackup-cookie"
                    , "http://download.oracle.com/otn-pub/java/jdk/7u76-b13/jdk-7u76-linux-x64.rpm"
                    , "-O", "jdk7.rpm"]

    install = [ "yum", "install", "-y", "jdk7.rpm" ]
    rm      = [ "rm", "jdk7.rpm" ]
