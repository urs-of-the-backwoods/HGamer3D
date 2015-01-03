-- This source file is part of HGamer3D
-- (A project to enable 3D game development in Haskell)
-- For the latest info, see http://www.hgamer3d.org
--
-- (c) 2014 Peter Althainz
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

-- RotatePlaton.hs
-- Setup Basic Elements

module Main

where

-- initialization

-- import the needed Ecs API files
import HGamer3D.Data
import HGamer3D.Common
import HGamer3D.Graphics3D

-- other imports
import Control.Concurrent

-- define a camera
camera1 = Camera (Frustum 5.0 5000.0 (Deg 40)) (Viewport 0 (Rectangle 0.0 0.0 1.0 1.0) black)

-- define some light and the geometry
light1 = Light white white PointLight 
platon = SimpleFigure Dodekaeder (ResourceMaterial "Colours/Red")

-- rotation angle, depending on time
rotationAngle :: IO Float
rotationAngle = do
  t <- getTime
  let t' = fromIntegral ((msec t) `mod` 5000)
  let v = (t' * 6.28 / 5000.0) 
  return v

-- main program
main = do

  -- make entities and hand it to the runtime systems

  -- camera, light and scene parameters into one
  envE <- newE [
       CTCam #: camera1,
       CTScP #: SceneParameter white NoShadows NoSky
       ]

  -- geometry and light entities
  geoE <- newE [
     CTPos #: Vec3 0.0 0.0 (-20.0),
     CTOri #: unitU,
     CTFig #: platon
     ]
  liE1 <- newE [
       CTLig #: light1,
       CTPos #: Vec3 500.0 500.0 (-500.0)
       ]
  liE2 <- newE [
       CTLig #: light1,
       CTPos #: Vec3 100.0 50.0 0.0	
       ]

  -- run graphics system
  world <- forkGraphics3DWorld (msecT 30)

  -- add entities to world
  mapM (addToWorld world) [envE, geoE, liE1, liE2] 

  -- implement quit handler
  qvar <- regQuitHandler envE

  let loop = do
            sleepFor (msecT 100)
            -- rotate geometry
	    rangle <- rotationAngle
	    updateE geoE CTOri (const (rotU vec3Y rangle))
      	    loop
  forkIO loop

  takeMVar qvar

