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

    , InotifyTools
    , inotifyTools

    , SHA256(..)
    , installRpm
    , installRpm'
    ) where

import qualified Data.Text as T
import           Data.Monoid ((<>))

import           Data.Dockerfile

------------------------------------------------------------------------

data Redhat
data Redhat6
data Redhat7

centos6 :: Dockerfile '[Redhat, Redhat6]
centos6 = base (RepoTag "centos" "6")

centos7 :: Dockerfile '[Redhat, Redhat7]
centos7 = base (RepoTag "centos" "7")

------------------------------------------------------------------------

data InotifyTools

inotifyTools :: Requires Redhat cs => Fragment cs (InotifyTools ': cs)
inotifyTools = installRpm url sha >>> addCapability
  where
    url = "http://dl.fedoraproject.org/pub/epel/6/x86_64/inotify-tools-3.14-1.el6.x86_64.rpm"
    sha = SHA256 "dbbf546acb824bf972433eb55f5ef9b4f16ad74833a5161d0916aa8c543b5bc7"

------------------------------------------------------------------------

newtype SHA256 = SHA256 T.Text

installRpm :: Requires Redhat cs => T.Text -> SHA256 -> Fragment cs cs
installRpm = installRpm' []

installRpm' :: Requires Redhat cs => [(T.Text, T.Text)] -> T.Text -> SHA256 -> Fragment cs cs
installRpm' cookies url (SHA256 sha) = runMany [writeSHA, fetch, showSHA, checkSHA, install, rm]
  where
    fetch = [ "curl", "--progress-bar"
                    , "-L", url
                    , "-o", name ] ++ cookieArgs

    cookieArgs | null cookies = []
               | otherwise    = [ "-b", T.intercalate ";"
                                      . map (\(n,v) -> n <> "=" <> v)
                                      $ cookies ]

    shaFile = name <> ".sha256"

    writeSHA = [ "echo", sha <> "  " <> name, ">", shaFile ]
    showSHA  = [ "sha256sum", name ]
    checkSHA = [ "sha256sum", "-c", shaFile ]

    install = [ "yum", "install", "-y", name ]
    rm      = [ "rm", name ]

    (_, name) = T.breakOnEnd "/" url
