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

# install stack
wget -q -O- https://s3.amazonaws.com/download.fpcomplete.com/ubuntu/fpco.key | apt-key add -
echo 'deb http://download.fpcomplete.com/ubuntu/vivid stable main'| tee /etc/apt/sources.list.d/fpco.list
$SUDO apt-get install libtinfo-dev -y
$SUDO apt-get install stack -y
stack setup --resolver lts-3.4

