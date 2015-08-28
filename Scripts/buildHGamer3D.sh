#!/bin/bash
# build hgamer3d in linux
# to be used as root
# assumption, current path is in HGamer3D

set -x
set -e

cd HGamer3D || true   # in case of travis use, we are already in HGamer3D, otherwise, we need to switch to it

mkdir Build
cd Build
mkdir Output
cp /usr/local/lib/Urho3D/libUrho3D.so Output
ln -s Output/libUrho3D.so Output/libUrho3D.so.0
cmake ../Source
export PATH=/.cabal/bin:$PATH
# following line only needed in Docker files
export LANG=C.UTF-8
export PATH=/home/travis/.cabal/bin:$PATH
# build hgamer3d
cmake --build . --config Release --target tests
cd ..

