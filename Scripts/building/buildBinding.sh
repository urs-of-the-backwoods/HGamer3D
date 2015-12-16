#!/bin/bash
# build binding in linux
# to be used as user
# to be called from HGamer3D directory

if [[ $EUID -ne 0 ]]; then
   SUDO="sudo"
else
   SUDO=""
fi

mkdir Binding-Build
cd Binding-Build
cmake ../Source
cmake --build . --config Release --target urho3dbinding_clib
$SUDO cmake --build . --config Release --target output_dll
$SUDO ldconfig

