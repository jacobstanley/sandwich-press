{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

{-# OPTIONS_GHC -w #-}

module Main where

import           Control.Monad.IO.Class (liftIO)
import           Data.Monoid ((<>))
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           GHC.TypeLits (Symbol)
import           Text.Shakespeare.Text (lbt)

import           Data.Dockerfile
import           Network.Docker

import           Docker.Redhat.Cloudera

------------------------------------------------------------------------

main :: IO ()
main = runDocker $ do
    --zki  <- build (zookeeper :: Dockerfile "centos" "6")
    --hdi  <- build (hadoop    :: Dockerfile "centos" "6")

    dnsi <- build (dnsmasq :: Dockerfile "centos" "6")
    dnsc <- createId <$> create (Just "dnsmasq") (containerFrom dnsi)
    start dnsc

    c <- getContainer dnsc
    liftIO (print c)

    return ()

------------------------------------------------------------------------

dnsmasq :: Dockerfile repo tag
dnsmasq = run ["yum", "install", "-y", "dnsmasq"]
