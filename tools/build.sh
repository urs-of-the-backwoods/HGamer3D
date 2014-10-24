#!/bin/bash
# shell script to build HGamer3D from source

cd Data
scons
cd ..

cd Bindings/Enet/Deps
scons
scons install
cd ../cBuild
scons
scons install
cd ../hBuild
scons
cd ../../..
cd Network
scons
cd ..

cd Bindings/SFML/Deps
scons
scons install
cd ../cBuild
scons
scons install
cd ../hBuild
scons
cd ../../..
cd Audio
scons
cd ..
cd InputSystem
scons
cd ..

cd Bindings/Ogre/Deps
scons
scons install
cd ../cBuild
scons
scons install
cd ../hBuild
scons
cd ../../..
cd Graphics3D
scons
cd ..

cd Bindings/SDL2/Deps
scons
scons install
cd ../cBuild
scons
scons install
cd ../hBuild
scons
cd ../../..
cd WinEvent
scons
cd ..

cd Bindings/CEGUI/Deps
scons
scons install
cd ../cBuild
scons
scons install
cd ../hBuild
scons
cd ../../..
cd GUI
scons
cd ..

cd Main
scons
cd ..

cd Docs
./generate.sh
cd ..

cd Examples
scons
cd ..
