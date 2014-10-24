#!/bin/bash
# script for generating all documentation with haddock into this directory

# Data and Util module docs
cd ../Data && cabal haddock --haddock-options="-o ../Docs/Data -D ../Docs/ifData"

# Enet Bindings, Network
cd ../Bindings/Enet/hBuild && cabal haddock --haddock-options="-o ../../../Docs/Enet -D ../../../Docs/ifEnet -i ../Data,../../../Docs/ifData" 
cd ../..
cd ../Network && cabal haddock --haddock-options="-o ../Docs/Network -D ../Docs/ifNetwork -i ../Data,../Docs/ifData -i ../Enet,../Docs/ifEnet"

# Ogre Bindings, Graphics3D
cd ../Bindings/Ogre/hBuild && cabal haddock --haddock-options="-o ../../../Docs/Ogre -D ../../../Docs/ifOgre -i ../Data,../../../Docs/ifData"
cd ../..
cd ../Graphics3D && cabal haddock --haddock-options="-o ../Docs/Graphics3D -D ../Docs/ifGraphics3D -i ../Data,../Docs/ifData -i ../Ogre,../Docs/ifOgre"

# SDL2 Bindings, WinEvent
cd ../Bindings/SDL2/hBuild && cabal haddock --haddock-options="-o ../../../Docs/SDL2 -D ../../../Docs/ifSDL2 -i ../Data,../../../Docs/ifData"
cd ../..
cd ../WinEvent && cabal haddock --haddock-options="-o ../Docs/WinEvent -D ../Docs/ifWinEvent -i ../Data,../Docs/ifData -i ../SDL2,../Docs/ifSDL2"

# CEGUI Bindings, GUI
cd ../Bindings/CEGUI/hBuild && cabal haddock --haddock-options="-o ../../../Docs/CEGUI -D ../../../Docs/ifCEGUI -i ../Data,../../../Docs/ifData"
cd ../..
cd ../GUI && cabal haddock --haddock-options="-o ../Docs/GUI -D ../Docs/ifGUI -i ../Data,../Docs/ifData -i ../CEGUI,../Docs/ifCEGUI -i ../WinEvent,../Docs/ifWinEvent"

# SFML Bindings, Network, Audio
cd ../Bindings/SFML/hBuild && cabal haddock --haddock-options="-o ../../../Docs/SFML -D ../../../Docs/ifSFML -i ../Data,../../../Docs/ifData"
cd ../..
cd ../InputSystem && cabal haddock --haddock-options="-o ../Docs/InputSystem -D ../Docs/ifInputSystem -i ../Data,../Docs/ifData -i ../SFML,../Docs/ifSFML"
cd ../Audio && cabal haddock --haddock-options="-o ../Docs/Audio -D ../Docs/ifAudio -i ../Data,../Docs/ifData -i ../SFML,../Docs/ifSFML"

# Main
cd ../Main && cabal haddock --haddock-options="-o ../Docs/Main -D ../Docs/ifMain -i ../Data,../Docs/ifData -i ../GUI,../Docs/ifGUI -i ../WinEvent,../Docs/ifWinEvent -i ../Graphics3D,../Docs/ifGraphics3D -i ../Audio,../Docs/ifAudio -i ../Network,../Docs/ifNetwork -i ../InputSystem,../Docs/ifInputSystem"

# Put all together in Docs folder
cd ../Docs && haddock --gen-contents -o ../Docs -i Data,../Docs/ifData -i GUI,../Docs/ifGUI -i WinEvent,../Docs/ifWinEvent -i Graphics3D,../Docs/ifGraphics3D -i Audio,../Docs/ifAudio -i Network,../Docs/ifNetwork -i InputSystem,../Docs/ifInputSystem -i Main,../Docs/ifMain


