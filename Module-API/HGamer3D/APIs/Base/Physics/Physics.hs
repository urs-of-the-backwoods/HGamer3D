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

-- Physics.hs

-- | Physics functionality of Base API. This module is likely to change in the near future and not well documented.


module HGamer3D.APIs.Base.Physics.Physics

(
	Body (..),
	createSphereBody,
	createBoxBody,
	createCylinderBody,
	createCapsuleBody,
	createConeBody,
	createStaticPlaneBody,
	
	physicsSimulationStep,
	setBodyGravity,
	getBodyGravity,
	applyBodyImpulse,
	getBodyLinearVelocity,
	setBodyLinearVelocity
)

where

import GHC.Ptr

import HGamer3D.Data.HG3DClass
import HGamer3D.Data.Vector
import HGamer3D.APIs.Base.Graphics3D.Basic3D

import HGamer3D.Bindings.Bullet.ClassHG3DBulletMotionStateHandler as Motion
import HGamer3D.Bindings.Bullet.ClassHG3DBulletDefaultCreator as Bullet
import HGamer3D.Bindings.Bullet.ClassConeShapeX as Cone
import HGamer3D.Bindings.Bullet.ClassDynamicsWorld as World
import HGamer3D.Bindings.Bullet.ClassHG3DRigidBodyCreator as Rigids
import HGamer3D.Bindings.Bullet.ClassRigidBody as RBody

import HGamer3D.Bindings.Bullet.ClassPtr
import HGamer3D.Bindings.Bullet.Utils

-- here the physics includes


import HGamer3D.APIs.Base.Engine.Types

import Control.Monad.Trans
import Control.Monad.Reader

data Body = Body HG3DClass


createSphereBody :: Float -> Vec3 -> Quaternion -> Float -> MHGamer3D Body
createSphereBody mass position quat radius = do
	rs <- ask
	let ps = physicsSystem rs
	let phys = psPhysics ps
	let world = psWorld ps
	sphere <- liftIO $ Rigids.createSphere mass position quat radius
	liftIO $ World.addRigidBody world sphere
	return (Body sphere)

createBoxBody :: Float -> Vec3 -> Quaternion -> Vec3 -> MHGamer3D Body
createBoxBody mass position quat halfExtends = do
	rs <- ask
	let ps = physicsSystem rs
	let phys = psPhysics ps
	let world = psWorld ps
	box <- liftIO $ Rigids.createBox mass position quat halfExtends
	liftIO $ World.addRigidBody world box
	return (Body box)

createCylinderBody :: Float -> Vec3 -> Quaternion -> Vec3 -> MHGamer3D Body
createCylinderBody mass position quat halfExtends = do
	rs <- ask
	let ps = physicsSystem rs
	let phys = psPhysics ps
	let world = psWorld ps
	cylinder <- liftIO $ Rigids.createCylinder mass position quat halfExtends
	liftIO $ World.addRigidBody world cylinder
	return (Body cylinder)

createCapsuleBody :: Float -> Vec3 -> Quaternion -> Float -> Float -> MHGamer3D Body
createCapsuleBody mass position quat radius height = do
	rs <- ask
	let ps = physicsSystem rs
	let phys = psPhysics ps
	let world = psWorld ps
	capsule <- liftIO $ Rigids.createCapsule mass position quat radius height
	liftIO $ World.addRigidBody world capsule
	return (Body capsule)

createConeBody :: Float -> Vec3 -> Quaternion -> Float -> Float -> MHGamer3D Body
createConeBody mass position quat radius height = do
	rs <- ask
	let ps = physicsSystem rs
	let phys = psPhysics ps
	let world = psWorld ps
	cone <- liftIO $ Rigids.createCone mass position quat radius height
	liftIO $ World.addRigidBody world cone
	return (Body cone)

createStaticPlaneBody :: Float -> Vec3 -> Quaternion -> Vec3 -> MHGamer3D Body
createStaticPlaneBody mass position quat origin = do
	rs <- ask
	let ps = physicsSystem rs
	let phys = psPhysics ps
	let world = psWorld ps
	plane <- liftIO $ Rigids.createStaticPlane mass position quat origin
	liftIO $ World.addRigidBody world plane
	return (Body plane)

physicsSimulationStep :: Float -> Int -> Float -> MHGamer3D Int
physicsSimulationStep timeStep maxSubSteps fixedTimeStep = do
	rs <- ask
	let ps = physicsSystem rs
	let phys = psPhysics ps
	let world = psWorld ps
	val <- liftIO $ World.stepSimulation world  timeStep maxSubSteps fixedTimeStep
	return val

instance Position3D Body where

	position3D (Body body) = do
		pos <- liftIO $ Motion.getPosition body
		return pos
	positionTo3D (Body body) pos = do
		liftIO $ Motion.setPosition body pos

instance Orientation3D Body where

	orientation3D (Body body) = do
		dir <- liftIO $ Motion.getQuaternion body
		return $ (mkU . fromQ) dir
	orientationTo3D (Body body) dir = do
		liftIO $ Motion.setQuaternion body ((toQ . fromU) dir)
		
setBodyGravity :: Body -> Vec3 -> MHGamer3D ()
setBodyGravity (Body body) grav = do
	liftIO $ RBody.setGravity body grav
	return ()

getBodyGravity :: Body -> MHGamer3D Vec3
getBodyGravity (Body body) = do
	grav <- liftIO $ RBody.getGravity body
	return grav
	
applyBodyImpulse :: Body -> Vec3 -> Vec3 -> MHGamer3D ()
applyBodyImpulse (Body body) imp relpos = do
	liftIO $ RBody.applyImpulse body imp relpos
	
getBodyLinearVelocity :: Body -> MHGamer3D Vec3
getBodyLinearVelocity (Body body) = do
	vel <- liftIO $ RBody.getLinearVelocity body
	return vel
	
setBodyLinearVelocity :: Body -> Vec3 -> MHGamer3D ()
setBodyLinearVelocity (Body body) vel = do
	liftIO $ RBody.setLinearVelocity body vel
	return ()
	

