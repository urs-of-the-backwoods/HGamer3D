-- This source file is part of HGamer3D
-- (A project to enable 3D game development in Haskell)
-- For the latest info, see http://www.hgamer3d.org
--
-- (c) 2011-2013 Peter Althainz
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

import HGamer3D.Graphics3D

white :: Colour
white = (Colour 1.0 1.0 1.0 1.0)

renderLoop cube g3ds = do
   -- rotate 
  yaw3D cube (Rad 0.005) 
  roll3D cube (Rad 0.002)
  quit <- loopHGamer3D g3ds
  if quit then return () else renderLoop cube g3ds
   
main :: IO ()
main = do
  
        (g3ds, camera, viewport) <- initHGamer3D "HGamer3D - BlueCube Example" "DefaultSceneManager" True True
        
	-- camera position
	let pos = Vec3 5.0 5.0 80.0
        positionTo3D camera pos
	let at = Vec3 0.0 0.0 (-300.0)
        cameraLookAt camera at
	
	-- define light
            
	setAmbientLight g3ds white
	pointlight g3ds white (Vec3 10.0 10.0 20.0)
        let cube = cubeMesh
        cube <- object3DFromMesh g3ds cube (Just (ResourceMaterial "Examples/Blue") )

	-- create a shiny blue cube
        positionTo3D cube (Vec3 0.0 0.0 0.0)
        scaleTo3D cube (Vec3 0.2 0.2 0.2)
        
	-- start render loop
	renderLoop cube g3ds 
        exitHGamer3D g3ds
        return ()

