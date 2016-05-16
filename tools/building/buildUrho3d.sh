#!/bin/bash
# build urho3d in linux
# shell script to load all pre-requisites and build urho3d
# builds version 5e7a3b12 (latest checkout from 16.5., version 1.5 +)

if [[ $EUID -ne 0 ]]; then
   SUDO="sudo"
else
   SUDO=""
fi
# set flags for error messages
set -x
set -e
# get software for linux pre-requisites
$SUDO apt-get -y install git build-essential cmake
$SUDO apt-get -y install mesa-common-dev freeglut3-dev libglew-dev libxrandr-dev libasound2-dev libpulse-dev libesd0-dev
# get Urho3D, version 1.5 + , tag 5e7a3b12
git clone https://github.com/urho3d/Urho3D
cd Urho3D
git checkout 5e7a3b12
# build urho3d
cd ..
mkdir Urho3D-Build
cd Urho3D-Build
cmake ../Urho3D -DURHO3D_LIB_TYPE=SHARED -DENABLE_SAMPLES=0 -DENABLE_TOOLS=0
cmake --build . --config Release
# install urho 3d
$SUDO cmake --build . --config Release --target install
cd ..

