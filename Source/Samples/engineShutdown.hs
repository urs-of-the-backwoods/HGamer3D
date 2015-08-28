-- This source file is part of HGamer3D
-- (A project to enable 3D game development in Haskell)
-- For the latest info, see http://www.hgamer3d.org
--
-- (c) 2015 Peter Althainz
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- You may obtain a copy of the License at
--
--     http://www.apache.org/licenses/LICENSE-2.0
--
-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS,
-- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
-- See the License for the specific language governing permissions and
-- limitations under the License.

-- Test for engine shutdown, being done properly

{-# LANGUAGE OverloadedStrings #-}

module Main where

import HGamer3D

import qualified Data.Text as T
import Control.Concurrent
import Control.Monad

start = do


      -- create G3D System
      eG3D <- newE [
            ctGraphics3DConfig #: standardGraphics3DConfig,
            ctGraphics3DCommand #: NoCmd
            ]
      world <- forkGraphics3DWorld (setC eG3D ctGraphics3DCommand Step >> return False) (msecT 20)

      -- wait until everything is initialized
      print "after press, system will initialize" 
      l <- getLine
      addToWorld world eG3D

      print "after press, system will shutdown"
      l <- getLine
      removeFromWorld world eG3D

      print "after press, system will initialize again"
      l <- getLine
      addToWorld world eG3D


      print "after press, graphics will initialize"
      l <- getLine

      -- create camera
      eCam <- newE [ 
            ctCamera #: MainCamera ,
            ctPosition #: Vec3 0 0 (-20.0),
            ctLight #:  PointLight (LightParameter 1.0 1000.0 1.0 False)
            ]
      addToWorld world eCam

      -- create Geometry
      eGeo <- newE [
            ctGeometry #: ShapeGeometry Cube,
            ctMaterial #: ResourceMaterial "Materials/Stone.xml",
            ctScale #: Vec3 2.0 2.0 2.0,
            ctPosition #: Vec3 0.0 0.0 0.0,
            ctOrientation #: unitU
            ]
      addToWorld world eGeo

      return eGeo

loop eGeo = forever $ do 
            updateC eGeo ctOrientation (\u -> (rotU vec3Y 0.02) .*. u)
            sleepFor (msecT 20)

demo = do
      eGeo <- start
      forkIO (loop eGeo)

main = do
      eGeo <- start
      loop eGeo
