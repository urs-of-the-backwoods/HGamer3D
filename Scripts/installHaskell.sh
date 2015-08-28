#!/bin/bash
# install Haskell in linux
# to be used as root

set -x
set -e

apt-get update -qq
apt-get -y install haskell-platform
cabal update
export PATH=/.cabal/bin:/home/travis/.cabal/bin:$PATH
cabal install cabal-install
cabal install c2hs
