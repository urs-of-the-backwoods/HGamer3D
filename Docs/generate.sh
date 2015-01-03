#!/bin/bash
# script for generating all documentation with haddock into this directory

# Data and Common module docs
cd ../Data && cabal haddock --haddock-options="-o ../Docs/Data -D ../Docs/ifData"
cd ../Common && cabal haddock --haddock-options="-o ../Docs/Common -D ../Docs/ifCommon -i ../Docs/ifData"

# Bindings
cd ../Bindings/Ogre/hBuild && cabal haddock --haddock-options="-o ../../../Docs/Ogre -D ../../../Docs/ifOgre -i ../../../Docs/ifData -i ../../../Docs/ifCommon" 
cd ../../../Bindings/CEGUI/hBuild && cabal haddock --haddock-options="-o ../../../Docs/CEGUI -D ../../../Docs/ifCEGUI -i ../../../Docs/ifData -i ../../../Docs/ifCommon" 
cd ../../../Bindings/SFML/hBuild && cabal haddock --haddock-options="-o ../../../Docs/SFML -D ../../../Docs/ifSFML -i ../../../Docs/ifData -i ../../../Docs/ifCommon" 
cd ../../../Bindings/SDL2/hBuild && cabal haddock --haddock-options="-o ../../../Docs/SDL2 -D ../../../Docs/ifSDL2 -i ../../../Docs/ifData -i ../../../Docs/ifCommon" 
cd ../../../Bindings/Enet/hBuild && cabal haddock --haddock-options="-o ../../../Docs/Enet -D ../../../Docs/ifEnet -i ../../../Docs/ifData -i ../../../Docs/ifCommon" 

# Network
cd ../../../Network && cabal haddock --haddock-options="-o ../Docs/Network -D ../Docs/ifNetwork -i ../Docs/ifData -i ../Docs/ifCommon -i ../Docs/ifEnet"

# Audio
cd ../Audio && cabal haddock --haddock-options="-o ../Docs/Audio -D ../Docs/ifAudio -i ../Docs/ifData -i ../Docs/ifCommon -i ../Docs/ifSFML"

# InputSystem
cd ../InputSystem && cabal haddock --haddock-options="-o ../Docs/InputSystem -D ../Docs/ifInputSystem -i ../Docs/ifData -i ../Docs/ifCommon -i ../Docs/ifSFML"

# Graphics3D
cd ../Graphics3D && cabal haddock --haddock-options="-o ../Docs/Graphics3D -D ../Docs/ifGraphics3D -i ../Docs/ifData -i ../Docs/ifCommon -i ../Docs/ifSDL2 -i ../Docs/ifCEGUI -i ../Docs/ifOgre"

# Main
cd ../Main && cabal haddock --haddock-options="-o ../Docs/Main -D ../Docs/ifMain -i ../Docs/ifData -i ../Docs/ifCommon -i ../Docs/ifGraphics3D -i ../Docs/ifAudio -i ../Docs/ifNetwork -i ../Docs/ifInputSystem"

# Put all together in Docs folder
cd ../Docs && haddock --gen-contents -o ../Docs -i Main,../Docs/ifMain -i Data,../Docs/ifData -i Common,../Docs/ifCommon -i Graphics3D,../Docs/ifGraphics3D -i Audio,../Docs/ifAudio -i Network,../Docs/ifNetwork -i InputSystem,../Docs/ifInputSystem


