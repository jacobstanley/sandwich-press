{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

{-# OPTIONS_GHC -w #-}

module Main where

import           Control.Monad.IO.Class (liftIO)

import           Data.Dockerfile
import           Network.Docker

import           Docker.Redhat
import           Docker.Redhat.Java
import           Docker.Redhat.Cloudera

------------------------------------------------------------------------

main :: IO ()
main = runDocker $ do
    zki  <- startZookeeperCluster 3

    --hdi  <- build (hadoop    :: Dockerfile "centos" "6")

    --dnsi <- build (from centos6 dnsmasq)
    --dnsc <- createId <$> create (Just "dnsmasq") (containerFrom dnsi)
    --start dnsc

    --c <- getContainer dnsc
    --liftIO (print c)

    return ()

------------------------------------------------------------------------

data Dnsmasq

dnsmasq :: Requires Redhat cs => Fragment cs (Dnsmasq ': cs)
dnsmasq = run ["yum", "install", "-y", "dnsmasq"]
      >>> addCapability
