#!/bin/bash
# install Haskell in linux
# to be used as root

set -x
set -e

# install stack
wget -q -O- https://s3.amazonaws.com/download.fpcomplete.com/ubuntu/fpco.key | apt-key add -
echo 'deb http://download.fpcomplete.com/ubuntu/vivid stable main'| tee /etc/apt/sources.list.d/fpco.list
apt-get update 
apt-get install libtinfo-dev -y
apt-get install stack -y
stack setup --resolver lts-3.4

