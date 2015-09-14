#!/bin/bash
# get hgamer3d in linux
# to be used as root

set -x
set -e

stack exec -- git clone https://github.com/urs-of-the-backwoods/HGamer3D
cd HGamer3D
