{-# LANGUAGE OverloadedStrings #-}

module Docker.Consul where

import qualified Data.Map as M
import           Data.Monoid ((<>))
import qualified Data.Text as T

import           Network.Docker

------------------------------------------------------------------------

type ServerCount = Int

------------------------------------------------------------------------

consulContainer :: CreateContainer RepoTag
consulContainer = containerFrom (RepoTag "progrium/consul" "latest")

------------------------------------------------------------------------

startBootstrapServer :: ServerCount -> ContainerName -> Docker (Id, IPAddress)
startBootstrapServer count name = do
    cid <- createId <$> create (Just name) consulContainer {
          createHostName     = name
        , createCommand      = ["-server", "-bootstrap-expect", T.pack (show count)]
        }

    start cid

    container <- getContainer cid
    let ip = networkIPAddress (containerNetwork container)

    return (cid, ip)

startServer :: IPAddress -> ContainerName -> Docker Id
startServer bootstrapIP name = do
    cid <- createId <$> create (Just name) consulContainer {
          createHostName   = name
        , createCommand    = ["-server", "-join", unIP bootstrapIP]
        }

    start cid
    return cid

startClient :: IPAddress -> ContainerName -> Docker Id
startClient bootstrapIP name = do
    cid <- createId <$> create (Just name) consulContainer {
          createHostName     = name
        , createCommand      = ["-join", unIP bootstrapIP, "-ui-dir", "/ui"]
        , createPortBindings = M.fromList [ (SrcPort 8400 TCP, [DstPort 8400 Nothing])
                                          , (SrcPort 8500 TCP, [DstPort 8500 Nothing])
                                          , (SrcPort 53   UDP, [DstPort 53   Nothing])
                                          ]
        }

    start cid
    return cid

------------------------------------------------------------------------

startCluster :: ServerCount -> Docker [Id]
startCluster count = do
    (server0, ip) <- startBootstrapServer count "consul0"

    let names = map (\x -> "consul" <> T.pack (show x)) [1..count-1]
    servers <- mapM (startServer ip) names

    web <- startClient ip "consulweb"

    return ([server0] ++ servers ++ [web])
