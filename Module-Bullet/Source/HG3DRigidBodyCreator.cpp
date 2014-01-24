/*
This source file is part of HGamer3D
(A project to enable 3D game development in Haskell)
For the latest info, see http://www.althainz.de/HGamer3D.html

(c) 2011 Peter Althainz

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
*/

#include <btBulletDynamicsCommon.h>
#include <HG3DRigidBodyCreator.h>


btRigidBody* HG3DRigidBodyCreator::createRigidBody(btScalar mass, btCollisionShape* shape, const btVector3* pos, const btQuaternion* qt)
{
	btVector3 intertia(0,0,0);
    shape->calculateLocalInertia(mass,intertia);
	btDefaultMotionState* motionState = new btDefaultMotionState(btTransform(*qt,*pos));
	btRigidBody::btRigidBodyConstructionInfo rigidBodyCI(mass,motionState,shape,intertia);
	btRigidBody* rigidBody = new btRigidBody(rigidBodyCI);
	return rigidBody;
};

// Sphere
btRigidBody* HG3DRigidBodyCreator::createSphere(btScalar mass, const btVector3* pos, const btQuaternion* qt, float radius)
{
	btCollisionShape *shape = new btSphereShape(radius);
	return createRigidBody(mass, shape, pos, qt);
};

// Box
btRigidBody* HG3DRigidBodyCreator::createBox(btScalar mass, const btVector3* pos, const btQuaternion* qt, const btVector3* boxHalfExtents)
{
	btCollisionShape *shape = new btBoxShape(*boxHalfExtents);
	return createRigidBody(mass, shape, pos, qt);
};

// Cylinder
btRigidBody* HG3DRigidBodyCreator::createCylinder(btScalar mass, const btVector3* pos, const btQuaternion* qt, const btVector3* boxHalfExtents)
{
	btCollisionShape *shape = new btCylinderShape(*boxHalfExtents);
	return createRigidBody(mass, shape, pos, qt);
};

// Capsule
btRigidBody* HG3DRigidBodyCreator::createCapsule(btScalar mass, const btVector3* pos, const btQuaternion* qt, btScalar radius, btScalar height)
{
	btCollisionShape *shape = new btCapsuleShape(radius, height);
	return createRigidBody(mass, shape, pos, qt);
};

// Cone
btRigidBody* HG3DRigidBodyCreator::createCone(btScalar mass, const btVector3* pos, const btQuaternion* qt, btScalar radius, btScalar height)
{
	btCollisionShape *shape = new btConeShape(radius, height);
	return createRigidBody(mass, shape, pos, qt);
};

// Static Plane
btRigidBody* HG3DRigidBodyCreator::createStaticPlane(btScalar mass, const btVector3* pos, const btQuaternion* qt, const btVector3* normal)
{
	btCollisionShape *shape = new btStaticPlaneShape(*normal,0.0);
	return createRigidBody(mass, shape, pos, qt);
};


