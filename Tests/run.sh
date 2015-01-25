#!/bin/bash
PATH=$PATH:$APPDATA/HGamer3D/lib
LD_LIBRARY_PATH=~/.HGamer3D/lib ./build/$1 $2 $3 $4 +RTS -N -RTS

