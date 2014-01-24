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
#include <HG3DMotionStateHandler.h>

btMotionState* HG3DMotionStateHandler::createDefaultMotionState(const btVector3* pos, const btQuaternion* qt)
{
	return (new btDefaultMotionState(btTransform(*qt,*pos)));
};

btVector3 HG3DMotionStateHandler::getPosition(const btRigidBody* rb)
{
	btTransform trans;
	rb->getMotionState()->getWorldTransform(trans);
	return trans.getOrigin();
};

void HG3DMotionStateHandler::setPosition(const btRigidBody* rb, const btVector3* pos)
{
	btTransform trans;
	rb->getMotionState()->getWorldTransform(trans);
	trans.setOrigin(*pos);
	((btRigidBody*)rb)->proceedToTransform(trans);
};

btQuaternion HG3DMotionStateHandler::getQuaternion(const btRigidBody* rb)
{
	btTransform trans;
	rb->getMotionState()->getWorldTransform(trans);
	return trans.getRotation();
};

void HG3DMotionStateHandler::setQuaternion(const btRigidBody* rb, const btQuaternion* rot)
{
	btTransform trans;
	rb->getMotionState()->getWorldTransform(trans);
	trans.setRotation(*rot);
	((btRigidBody*)rb)->proceedToTransform(trans);
};

