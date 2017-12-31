# HGamer3D

A tool to use Haskell for game programming. See the [HGamer3D website](http://www.hgamer3D.org) .

You do not need to clone this repo, to use it, instead use instructions below.

## install to use as library

- First install aio, see [aio installer](http://github.com/urs-of-the-backwoods/aio-installer).
- Then run the `aio http://www.hgamer3d.org/tools/Install.1217 install` command.

This will setup HGamer3D libraries and tools and enables you to:

- create new projects, with the `aio Create <name>` command
- build projects, by issuing the `aio Stack install --local-bin-dir .` command
- run projects, by issuing the `aio Run ./game` command
- edit files, by issuing the `aio Edit <filename>` command
- edit 3D scenes, by issuing the `aio 3DEdit` command (thanks to Urho3D editor)
- import 3D assets, by issuing the `aio AssetImporter` command (thanks to Urho3D team)

## this repo

This repo contains all the source code for the specific Haskell binding. 

regards
uotbw

