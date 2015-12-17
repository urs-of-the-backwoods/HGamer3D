#!/bin/bash
# build hgamer3d in linux
# to be used as root or user
# builds from any path, first parameter needs to be path to HGamer3D

set -x
set -e

cd ..
mkdir HGamer3D-Build
cd HGamer3D-Build

export LANG=C.UTF-8     # needed for docker files
stack build c2hs --resolver lts-3.4
cmake $1/Source
cmake --build . --config Release --target samples

