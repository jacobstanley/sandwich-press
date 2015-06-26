{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Docker.Redhat.Cloudera where

import qualified Data.Text.Lazy as LT

import Data.Dockerfile
--import Docker.Remote

import Docker.Redhat
import Docker.Redhat.Java

------------------------------------------------------------------------
-- ZooKeeper

zookeeper :: Redhat repo => Dockerfile repo tag
zookeeper = oracleJDK7 <> clouderaRepos
         <> run [ "yum", "install", "-y", "zookeeper-server" ]
         <> run [ "mkdir", "-p", "/var/lib/zookeeper" ]
         <> run [ "chown", "-R", "zookeeper", "/var/lib/zookeeper" ]

--startZookeeperCluster :: Int -> 

--service zookeeper-server init --myid=1
--service zookeeper-server start
--
--tail -F /var/log/zookeeper/zookeeper.log


zkCfg :: LT.Text
zkCfg = [lbt|tickTime=2000
            |dataDir=/var/lib/zookeeper/
            |clientPort=2181
            |initLimit=5
            |syncLimit=2
            |server.1=zoo1:2888:3888
            |server.2=zoo2:2888:3888
            |server.3=zoo3:2888:3888
            |]

------------------------------------------------------------------------
-- Hadoop

hadoop :: Redhat repo => Dockerfile repo tag
hadoop = oracleJDK7 <> clouderaRepos
      <> run [ "yum", "install", "-y", "hadoop" ]

------------------------------------------------------------------------
-- Repos

clouderaRepos :: Redhat repo => Dockerfile repo tag
clouderaRepos = copyUtf8 0o644 "/etc/yum.repos.d/cloudera.repo"        repo
             <> copyUtf8 0o644 "/etc/pki/rpm-gpg/RPM-GPG-KEY-cloudera" gpg
             <> run [ "yum", "clean", "all" ]
  where
    repo = [lbt|[cloudera-cdh5]
               |name=Cloudera's Distribution for Hadoop, Version 5.3.3
               |baseurl=http://archive.cloudera.com/cdh5/redhat/6/x86_64/cdh/5.3.3/
               |gpgkey=file:///etc/pki/rpm-gpg/RPM-GPG-KEY-cloudera
               |gpgcheck=1
               |]

    gpg = [lbt|-----BEGIN PGP PUBLIC KEY BLOCK-----
              |Version: GnuPG v1.4.5 (GNU/Linux)
              |
              |mQGiBEpBgEURBAC+CL1a6BfVEoKAX1KcOHqq9Z10WdPGOgTM+AtnOVPJdJvIZcDk
              |YGUmycpaGxY3+xX1x8ZvxNb7WXiei8FMPm4sR/xQC/CF2iS5399tjLJqcDEjdqTV
              |/whQ4Rrg1JLGaHUjR0YmrOteT71xikEwlCalToxQuhBz7Nz4aBeDDPf9lwCgvG+x
              |CaOxict+He03g4HNSTZ0T0UEAIxKITpCA6ZvUPoEGhpn+Gt+wJK/ScB0FKCfW8Au
              |QQZP6tgxDEg0baasT8MxuXXE2+opaaWPTVa64ws7OvbyH5z1xhBOx4qRVBx8bZsF
              |YQUk/1PBvg6yA4Rmaqi7nTToHatP69/JMLfTyH8sXETMQ8z5T0LAD6a5ELAYBqql
              |bJWRA/4lkbaGIwkyLcOAop/g0SCERHt66ML1pwdjxvzE2rRKFUbjUbRZsHTqVq5E
              |BgpcTIeTuRy02yQ+Bh+JaBtYhn0AY5+t7jcCdJeTahS/7RKJPYPiSfbgI6zwpHM9
              |kX4FT+0yDgnVF1H/h9p19Uv/3ahIgt7op/M1eAdH0/eP6Dv04rQnWXVtIE1haW50
              |YWluZXIgPHdlYm1hc3RlckBjbG91ZGVyYS5jb20+iGAEExECACAFAkpBgEUCGwMG
              |CwkIBwMCBBUCCAMEFgIDAQIeAQIXgAAKCRD5DA2P6PhqzRo1AKCIHNWJSd7OipbZ
              |qp58f/BWaIBlDACggNRH4Hvg92t3xtwYFdohRWF2Xbi5Ag0ESkGARxAIAMaPPGfQ
              |vsLkyLyM3ePtkkHi0bew0XGW1CYxWOZLMu8wnJgMHpfPD2dLgp6PEh+zpi2SM1ie
              |QGAW6K040TSuC9P+LcZB7SxanIE7lONHjz7spGQift30WFZcaIgF+MuyZIihNh7v
              |tZ9ip8JZYPA88XRNU1CKuXx4r8iCDJ4ICksFKeOwQUuzf/IRJapzEZ0ixfVTwx91
              |yG10TvHK63BRLXYHBML4Og9FaPZgFq2N9Yz4Wpu/Pn6tjZAMeSJXm2qNO2PSoTC/
              |kapubpMwSmOBlZqrHi9lcIWricXE9dcyaGVRAf3CJRlX4ZNuwcQjyks5BFibU3/z
              |qlzP6KgwTgDmaaMAAwUH/04KRM3k6Ow2KkDt2BKWveOI24mkIQahUJ7/iZlKsL27
              |3VcGQZ7jU28GT0FH9iYeAgbpLrrEuDAFZpGm9RoOVJGnxWX3DVL1+qkiS56pXfU+
              |8atZlkCGx09IilJgf0ATlmYxbTtYliTRPK4lQYOfNB1v23bdlBwISjcDRkWu22ao
              |atSBzr/FARL6fdZZqp2qfWOmcteiLagioo6s0ogxKNQH5PldUQy9n2W/oOXss5sC
              |lnUNvzKlzzx/pFkT8ZUAvuLY0v8gykk586vbjiuPkg8uAOBhtnsSWwJ6nEPaRCnu
              |iwlqGxgXmnJ7UMzOimkuf0XvqavhkMEEAqRJkNLyWVuISQQYEQIACQUCSkGARwIb
              |DAAKCRD5DA2P6PhqzUV2AJ0eV3C407Y3Xi4d27clLsz/wW0HMgCghcxCmiOT2kWH
              |6Ya7d9nkKz2UM+Y=
              |=+VR8
              |-----END PGP PUBLIC KEY BLOCK-----
              |]
