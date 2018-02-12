# HGamer3D

A tool to use Haskell for game programming. See the [HGamer3D website](http://www.hgamer3D.org) .

You do not need to clone this repo, to use it, instead use instructions below.

## start sample browser

- First install aio, see [aio installer](http://github.com/urs-of-the-backwoods/aio-installer).
- Then run the `aio http://www.hgamer3d.org/tools/HGamer3D sample-browser` command.

This will start the sample browswer, without installing the tool stack.

## install tools

- First install aio, see [aio installer](http://github.com/urs-of-the-backwoods/aio-installer).
- Then run the `aio http://www.hgamer3d.org/tools/HGamer3D install` command.

This will setup Haskell tools and some shortcuts, needed for next steps, see next section.

## compile and run an example program

After you started the install above, you can:

- create new projects, with the `aio HGamer3D create <project-dir>` command
- build those projects within their directory with the `build` script
- run projects, by using the `run`script

## addditional tools

The install will also give you shorcuts for:

- edit files, by issuing the `aio Edit <filename>` command
- edit 3D scenes, by issuing the `aio 3DEdit` command (thanks to Urho3D editor)
- import 3D assets, by issuing the `aio AssetImporter` command (thanks to Urho3D team)

## this repo

This repo contains all the source code for the specific Haskell binding. 

regards
uotbw

