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
#include <HG3DBulletDefaultCreator.h>


HG3DBulletDefaultCreator::HG3DBulletDefaultCreator()
{
	m_broadphase = NULL;
	m_collisionConfiguration = NULL;
	m_dispatcher = NULL;
	m_solver = NULL;
	m_dynamicsWorld = NULL;
};

HG3DBulletDefaultCreator::~HG3DBulletDefaultCreator()
{
	delete m_dynamicsWorld;
    delete m_solver;
    delete m_dispatcher;
    delete m_collisionConfiguration;
    delete m_broadphase;
};

void HG3DBulletDefaultCreator::initializeDefaults()
{
	if (m_dynamicsWorld == NULL)
	{
		m_broadphase = new btDbvtBroadphase();
		m_collisionConfiguration = new btDefaultCollisionConfiguration();
		m_dispatcher = new btCollisionDispatcher(m_collisionConfiguration);
		m_solver = new btSequentialImpulseConstraintSolver();
		m_dynamicsWorld = new btDiscreteDynamicsWorld(m_dispatcher,m_broadphase,m_solver,m_collisionConfiguration);
		m_dynamicsWorld->setGravity(btVector3(0,-10,0));
	}
};

btDynamicsWorld* HG3DBulletDefaultCreator::getDynamicsWorld()
{
	if (m_dynamicsWorld == NULL)
	{
		this->initializeDefaults();
	}
	return m_dynamicsWorld;
};

btCollisionDispatcher* HG3DBulletDefaultCreator::getCollisionDispatcher()
{
	if (m_dynamicsWorld == NULL)
	{
		this->initializeDefaults();
	}
	return m_dispatcher;
};

