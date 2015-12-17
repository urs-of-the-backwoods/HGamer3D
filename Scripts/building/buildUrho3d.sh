#!/bin/bash
# build urho3d in linux
# shell script to load all pre-requisites and build urho3d
# works in any path, creates 1.4.tar.gz, Urho3D-1.4 and Urho3D-Build in this path

if [[ $EUID -ne 0 ]]; then
   SUDO="sudo"
else
   SUDO=""
fi
# set flags for error messages
set -x
set -e
# get software for linux pre-requisites
$SUDO apt-get -y install wget unzip cmake ca-certificates
$SUDO apt-get -y install build-essential mesa-common-dev freeglut3-dev libglew-dev libxrandr-dev libasound2-dev
$SUDO ln -s /usr/include/GL /usr/include/GLEW
# get Urho3D, version 1.4, tested
wget https://github.com/urho3d/Urho3D/archive/1.4.tar.gz
tar -xzf 1.4.tar.gz
# build urho 3d
cd Urho3D-1.4
./cmake_generic.sh ../Urho3D-Build -DURHO3D_LIB_TYPE=SHARED
cd ../Urho3D-Build
cmake --build . --config Release
# install urho 3d
$SUDO cmake --build . --config Release --target install
cd ..

