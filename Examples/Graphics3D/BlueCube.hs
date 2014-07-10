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

import HGamer3D.BaseAPI

renderLoop cube g3ds guis = do
   -- rotate 
  yaw cube (Rad 0.005) 
  roll cube (Rad 0.002)
  (ev, quit) <- stepHGamer3D g3ds guis
  if quit then return () else renderLoop cube g3ds guis
   
main :: IO ()
main = do
  
        (g3ds, guis, camera, viewport) <- initHGamer3D "HGamer3D - BlueCube Example" True True True
        
	-- camera position
	let pos = Vec3 5.0 5.0 80.0
        positionTo camera pos
	let at = Vec3 0.0 0.0 (-300.0)
        cameraLookAt camera at
	
	-- define light
            
	setAmbientLight g3ds white
	pointLight g3ds white (Vec3 10.0 10.0 20.0)
        
	-- create a shiny blue cube        
        let blueMaterial = resourceMaterial "Colours/Blue"
--        cube <- object3DFromMesh g3ds cubeMesh (Just blueMaterial) False
        cube <- object3DFromMesh g3ds cubeMesh Nothing False
        positionTo cube (Vec3 0.0 0.0 0.0)
        scale cube (Vec3 0.2 0.2 0.2)
        
	-- start render loop
	renderLoop cube g3ds guis
        freeHGamer3D g3ds guis
        return ()

