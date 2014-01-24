-- This source file is part of HGamer3D
-- (A project to enable 3D game development in Haskell)
-- For the latest info, see http://www.althainz.de/HGamer3D.html
--
-- (c) 2011 Peter Althainz
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

-- EngineHelper.hs

-- | Internal functionality of pyhsics functionality for Base API.


module HGamer3D.APIs.Base.Physics.EngineHelper

(
		initPhysicsEngine
)

where

import GHC.Ptr

import HGamer3D.Data.HG3DClass
import HGamer3D.Data.Vector

import HGamer3D.Bindings.Bullet.ClassHG3DBulletMotionStateHandler as Motion
import HGamer3D.Bindings.Bullet.ClassHG3DBulletDefaultCreator as Bullet
import HGamer3D.Bindings.Bullet.ClassConeShapeX as Cone
import HGamer3D.Bindings.Bullet.ClassDynamicsWorld as World
import HGamer3D.Bindings.Bullet.ClassHG3DRigidBodyCreator as Rigids

import HGamer3D.Bindings.Bullet.ClassPtr
import HGamer3D.Bindings.Bullet.Utils

import HGamer3D.APIs.Base.Engine.Types

import Control.Monad.Trans
import Control.Monad.Reader


-- | initialize the physics system, only used internally in Base API.
initPhysicsEngine :: IO PhysicsSystem
initPhysicsEngine = do

	bullet <- Bullet.new
	world <- Bullet.getDynamicsWorld bullet
	let ps = (PhysicsSystem bullet world)
	return ps
