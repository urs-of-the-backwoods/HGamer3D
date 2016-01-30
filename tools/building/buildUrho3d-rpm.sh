#!/bin/bash
# build urho3d in linux (rpm based, fedora, centos, ...)
# shell script to load all pre-requisites and build urho3d
# to be used as root

set -x
set -e

# uncomment following lines for centos
# yum install epel-release -y
# yum install dnf -y

dnf install -y make automake gcc gcc-c++
dnf install -y wget unzip cmake
dnf install -y mesa-libGL-devel freeglut-devel glew-devel libXrandr-devel alsa-lib-devel
# ln -s /usr/include/GL /usr/include/GLEW

wget http://sourceforge.net/projects/urho3d/files/Urho3D/1.4/Urho3D-1.4-Source.zip
unzip Urho3D-1.4-Source.zip
mkdir Urho3D-Build
cd Urho3D-1.4
./cmake_generic.sh ../Urho3D-Build -DURHO3D_LIB_TYPE=SHARED
cd ../Urho3D-Build
cmake --build . --config Release --target install
cd ..
rm -rf Urho3D*

