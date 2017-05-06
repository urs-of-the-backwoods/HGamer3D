#!/bin/bash
# prepare host, to build urho3d and gamegio in linux

# prerequisites
if [[ $EUID -ne 0 ]]; then
   SUDO="sudo"
else
   SUDO=""
fi
# set flags for error messages
set -x
set -e

# get and install needed linux packages
$SUDO apt-get update
$SUDO apt-get -y install git build-essential cmake wget unzip
$SUDO apt-get -y install mesa-common-dev freeglut3-dev libglew-dev libxrandr-dev libasound2-dev libpulse-dev libesd0-dev

# create needed link for libraries, using Urho3D
$SUDO ln -s /usr/include/GL /usr/include/GLEW

