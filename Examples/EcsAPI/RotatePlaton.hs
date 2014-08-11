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
import HGamer3D.Engine.EcsAPI
import HGamer3D.Graphics3D.EcsAPI

-- other imports
import Control.Concurrent

-- define a camera
camera1 = Camera (Frustum 5.0 5000.0 (Deg 40)) (Viewport 0 (Rectangle 0.0 0.0 1.0 1.0) black)

-- define some light and the geometry
light1 = Light white white PointLight 
platon = SimpleFigure Dodekaeder (ResourceMaterial "Colours/Red")

-- define event listener, to get quit event
events1 = EventReceiver [ApplicationEvents]

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
  envE <- entity [
       CTCam #: camera1,
       CTScP #: SceneParameter white NoShadows NoSky,
       CTEvR #: events1
       ]

  -- geometry and light entities
  geoE <- entity [
     CTPos #: Vec3 0.0 0.0 (-20.0),
     CTOri #: unitU,
     CTFig #: platon
     ]
  liE1 <- entity [
       CTLig #: light1,
       CTPos #: Vec3 500.0 500.0 (-500.0)
       ]
  liE2 <- entity [
       CTLig #: light1,
       CTPos #: Vec3 100.0 50.0 0.0	
       ]

  -- run graphics system
  ecsG3D <- runSystemGraphics3D (msecT 30)
  ecsEvt <- runSystemEvent (msecT 110)
  let systems = ecsG3D #+ ecsEvt #+ []

  -- add entities to running system
  mapM (addToWorld systems) [envE, geoE, liE1, liE2] 

  -- run logic until quit (logis is to rotate the dodekaeder)

  -- run until quit
  let listenQuitLoop = do
      -- receive event and quit loop on AppQuit
      evts <- receiveEvents envE
      if length (filter (\evt -> case evt of
      	 		      	      (AppEvt AppQuit) -> True
				      _ -> False) evts) > 0 
         then return ()
         else do   
            -- wait 100 msec to save CPU time
            threadDelay 100000 
            -- rotate geometry
	    rangle <- rotationAngle
	    updateC (geoE # CTOri) (const (rotU vec3Y rangle))
      	    listenQuitLoop

  listenQuitLoop

