#!/bin/bash
# build hgamer3d in linux
# to be used as root or user
# builds from any path, first parameter needs to be path to HGamer3D

set -x
set -e

if [[ $EUID -ne 0 ]]; then
   SUDO="sudo"
else
   SUDO=""
fi

# install python doit tool
$SUDO apt-get install python-pip
$SUDO pip install doit

# clone HGamer3D
git clone https://github.com/urs-of-the-backwoods/HGamer3D

# compile gamegio, create engine component
cd HGamer3D
doit gamegio
cd components
doit engine

