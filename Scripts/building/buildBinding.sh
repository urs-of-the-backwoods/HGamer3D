#!/bin/bash
# build binding in linux
# to be used as user or root
# to be called from any directory, first parameter needs to be the HGamer3D directory

if [[ $EUID -ne 0 ]]; then
   SUDO="sudo"
else
   SUDO=""
fi

mkdir Binding-Build
cd Binding-Build
cmake $1/Source
cmake --build . --config Release --target urho3dbinding_clib
$SUDO cmake --build . --config Release --target output_dll
$SUDO ldconfig

