#!/bin/bash
# build hgamer3d in linux
# to be used as root
# assumption, current path is in HGamer3D

set -x
set -e

cd ..
mkdir BuildHG3D
cd BuildHG3D

export LANG=C.UTF-8     # needed for docker files
stack build c2hs --resolver lts-3.4
cmake /opt/hg3d/Source
cmake --build . --config Release --target samples

