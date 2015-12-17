#!/bin/bash
# install Haskell in linux
# to be used as root

set -x
set -e

if [[ $EUID -ne 0 ]]; then
   SUDO="sudo"
else
   SUDO=""
fi

$SUDO apt-get -y install wget ca-certificates libgmp-dev
wget https://www.stackage.org/stack/linux-x86_64 -O stack.tar.gz
tar -xzf stack.tar.gz
$SUDO mv stack-*/stack /usr/local/bin
stack setup --resolver lts-3.4

