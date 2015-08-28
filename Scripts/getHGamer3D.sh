#!/bin/bash
# get hgamer3d in linux
# to be used as root

set -x
set -e

apt-get -y install git
git clone https://github.com/urs-of-the-backwoods/HGamer3D
