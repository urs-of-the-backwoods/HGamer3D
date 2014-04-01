// This source file is part of HGamer3D, a project to enable 3D game development 
// in Haskell. For the latest info, see http://www.hgamer3d.org .
// 
// (c) 2011-2014 Peter Althainz
// 
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
// 
//     http://www.apache.org/licenses/LICENSE-2.0
// 
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.
// 

// ClassControllerManager.cpp

// 

#include <wchar.h>
#include <string>
#include <iostream>

#include <iostream>
	#include <typeinfo>
	#include <stdio.h>
	#include <cstring>
	#include <exception>
	#include "OgreDllDefines.h"
	#include "ClassPtr.h"
	#include "./Ogre.h"
#include "./OgreString.h"
#include "./WindowUtilsHG3D.h"
#include "HG3DUtilities.h"

using namespace Ogre;
using Ogre::uint;



// 
extern "C" Ogre_LIB_EXPORT void ogre_cmgr_construct(struct hg3dclass_struct * result_c)
{
  Ogre::ControllerManager * result_cpp;
  result_cpp = (new Ogre::ControllerManager());
  *result_c = getHG3DClass_ControllerManager((void *) result_cpp);
;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_cmgr_destruct(struct hg3dclass_struct * thisclass_c)
{
  Ogre::ControllerManager * thisclass_cpp = static_cast<Ogre::ControllerManager*> (getHG3DClassPtr(*thisclass_c, "Ogre::ControllerManager"));
  (delete thisclass_cpp);
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_cmgr_clearControllers(struct hg3dclass_struct * thisclass_c)
{
  Ogre::ControllerManager * thisclass_cpp = static_cast<Ogre::ControllerManager*> (getHG3DClassPtr(*thisclass_c, "Ogre::ControllerManager"));
  (thisclass_cpp->clearControllers());
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_cmgr_updateAllControllers(struct hg3dclass_struct * thisclass_c)
{
  Ogre::ControllerManager * thisclass_cpp = static_cast<Ogre::ControllerManager*> (getHG3DClassPtr(*thisclass_c, "Ogre::ControllerManager"));
  (thisclass_cpp->updateAllControllers());
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_cmgr_getTimeFactor(struct hg3dclass_struct * thisclass_c, float * result_c)
{
  Ogre::ControllerManager * thisclass_cpp = static_cast<Ogre::ControllerManager*> (getHG3DClassPtr(*thisclass_c, "Ogre::ControllerManager"));
  Real result_cpp;
  result_cpp = (thisclass_cpp->getTimeFactor());
  *result_c = (float)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_cmgr_setTimeFactor(struct hg3dclass_struct * thisclass_c, float tf_c)
{
  Ogre::ControllerManager * thisclass_cpp = static_cast<Ogre::ControllerManager*> (getHG3DClassPtr(*thisclass_c, "Ogre::ControllerManager"));
  Real tf_cpp = (Real)tf_c;
  (thisclass_cpp->setTimeFactor(tf_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_cmgr_getFrameDelay(struct hg3dclass_struct * thisclass_c, float * result_c)
{
  Ogre::ControllerManager * thisclass_cpp = static_cast<Ogre::ControllerManager*> (getHG3DClassPtr(*thisclass_c, "Ogre::ControllerManager"));
  Real result_cpp;
  result_cpp = (thisclass_cpp->getFrameDelay());
  *result_c = (float)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_cmgr_setFrameDelay(struct hg3dclass_struct * thisclass_c, float fd_c)
{
  Ogre::ControllerManager * thisclass_cpp = static_cast<Ogre::ControllerManager*> (getHG3DClassPtr(*thisclass_c, "Ogre::ControllerManager"));
  Real fd_cpp = (Real)fd_c;
  (thisclass_cpp->setFrameDelay(fd_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_cmgr_getElapsedTime(struct hg3dclass_struct * thisclass_c, float * result_c)
{
  Ogre::ControllerManager * thisclass_cpp = static_cast<Ogre::ControllerManager*> (getHG3DClassPtr(*thisclass_c, "Ogre::ControllerManager"));
  Real result_cpp;
  result_cpp = (thisclass_cpp->getElapsedTime());
  *result_c = (float)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_cmgr_setElapsedTime(struct hg3dclass_struct * thisclass_c, float elapsedTime_c)
{
  Ogre::ControllerManager * thisclass_cpp = static_cast<Ogre::ControllerManager*> (getHG3DClassPtr(*thisclass_c, "Ogre::ControllerManager"));
  Real elapsedTime_cpp = (Real)elapsedTime_c;
  (thisclass_cpp->setElapsedTime(elapsedTime_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_cmgr_getSingleton(struct hg3dclass_struct * result_c)
{
  Ogre::ControllerManager * result_cpp;
  result_cpp = &(Ogre::ControllerManager::getSingleton());
  *result_c = getHG3DClass_ControllerManager((void *) result_cpp);
;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_cmgr_getSingletonPtr(struct hg3dclass_struct * result_c)
{
  Ogre::ControllerManager * result_cpp;
  result_cpp = (Ogre::ControllerManager::getSingletonPtr());
  *result_c = getHG3DClass_ControllerManager((void *) result_cpp);
;
};

