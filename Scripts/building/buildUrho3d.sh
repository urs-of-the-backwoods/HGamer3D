#!/bin/bash
# build urho3d in linux
# shell script to load all pre-requisites and build urho3d
# to be used as root

set -x
set -e

apt-get update -qq
apt-get -y install wget unzip cmake
apt-get -y install build-essential mesa-common-dev freeglut3-dev libglew-dev libxrandr-dev libasound2-dev
ln -s /usr/include/GL /usr/include/GLEW

wget http://sourceforge.net/projects/urho3d/files/Urho3D/1.4/Urho3D-1.4-Source.zip
unzip Urho3D-1.4-Source.zip
mkdir Urho3D-Build
cd Urho3D-1.4
./cmake_generic.sh ../Urho3D-Build -DURHO3D_LIB_TYPE=SHARED
cd ../Urho3D-Build
cmake --build . --config Release --target install
cd ..
rm -rf Urho3D*

