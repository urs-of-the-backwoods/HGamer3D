-- this script creates a project

local os_name = require("os_name")
this_os, _ = os_name.getOS()


cabal_file = [[
name:                game
version:             0.1.0.0
synopsis:            game template
description:         Please see README.md
homepage:            https://github.com/githubuser/simple#readme
license:             BSD3
license-file:        LICENSE
author:              Author name here
maintainer:          example@example.com
copyright:           2016 Author name here
category:            Web
build-type:          Simple
-- extra-source-files:
cabal-version:       >=1.10

executable game
  hs-source-dirs:      .
  main-is:             game.hs
  ghc-options:         -threaded -rtsopts -with-rtsopts=-N
  build-depends:       base, text, HGamer3D >= {version_hgamer3d}
  default-language:    Haskell2010
]]

setup_file = [[
import Distribution.Simple
main = defaultMain
]]

license_file = [[
Copyright Author name here (c) 2016

All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

    * Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

    * Neither the name of Author name here nor the names of other
      contributors may be used to endorse or promote products derived
      from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
]]

yaml_file = [[
extra-deps: ["HGamer3D-{version_hgamer3d}", "fresco-binding-{version_fresco}", "vect-0.4.7"]
resolver: lts-5.8
flags: {}
packages: ["."]
]]

haskell_file = [[
{-# LANGUAGE OverloadedStrings #-}
module Main where

import HGamer3D

import qualified Data.Text as T
import Control.Concurrent
import Control.Monad
import System.Exit

start = do

    -- initialize system
    hg3d <- configureHG3D      -- initialize

    -- create minimum elements, like a camera
    eCam <- newE [
        ctCamera #: FullViewCamera,
        ctPosition #: Vec3 1 1 (-30.0),
        ctLight #: Light PointLight 1.0 1000.0 1.0 
        ]

    -- do something interesting here, in this example case, it is a text and
    -- a rotating cube

    eText <- newE [
        ctText #: "Rotating Cube Example",
        ctScreenRect #: Rectangle 10 10 100 25
        ]

    eGeo <- newE [
        ctGeometry #: ShapeGeometry Cube,
        ctMaterial #: matBlue,
        ctScale #: Vec3 10.0 10.0 10.0,
        ctPosition #: Vec3 0.0 0.0 0.0,
        ctOrientation #: unitU
        ]

    return (eGeo, hg3d)


rotateCube eGeo = do
    forever $ do 
        updateC eGeo ctOrientation (\u -> (rotU vec3Z 0.02) .*. u)
        updateC eGeo ctOrientation (\u -> (rotU vec3X 0.015) .*. u)
        sleepFor (msecT 12)
    return ()

main = do 
    (eGeo, hg3d) <- start
    forkIO $ rotateCube eGeo
    loopHG3D hg3d (msecT 20) (return True) -- allow close on windows click
    return ()
]]

local function writeIt(name, content) 
  fout = io.open(name, "w")
  io.output(fout)
  io.write(content)
  io.close(fout)
end

writeIt("stack.yaml", yaml_file)
writeIt("game.hs", haskell_file)
writeIt("LICENSE", license_file)
writeIt("Setup.hs", setup_file)
writeIt("game.cabal", cabal_file)

run_file = [[
aio http://www.hgamer3d.org/component/Run aio http://www.hgamer3d.org/component/Stack exec game
]]

if this_os == "Windows" then 
  writeIt("run.bat", run_file)
else 
  writeIt("run", run_file)
  os.execute("chmod +x run")  
end

repl_file = [[
aio http://www.hgamer3d.org/component/Run aio http://www.hgamer3d.org/component/Stack ghci
]]

if this_os == "Windows" then 
  writeIt("repl.bat", repl_file)
else 
  writeIt("repl", repl_file)
  os.execute("chmod +x repl")  
end

build_file = [[
aio http://www.hgamer3d.org/component/Stack build
]]

if this_os == "Windows" then 
  writeIt("build.bat", build_file)
else 
  writeIt("build", build_file)
  os.execute("chmod +x build")  
end

