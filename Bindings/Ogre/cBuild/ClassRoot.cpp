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

// ClassRoot.cpp

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
	#include "StructColour.h"
#include "./Ogre.h"
#include "./OgreString.h"
#include "./WindowUtilsHG3D.h"
#include "HG3DUtilities.h"

using namespace Ogre;
using Ogre::uint;



// 
extern "C" Ogre_LIB_EXPORT void ogre_rt_construct(char * pluginFileName_c, char * configFileName_c, char * logFileName_c, struct hg3dclass_struct * result_c)
{
  Ogre::String pluginFileName_cpp = Ogre::String((const char*) pluginFileName_c);
  Ogre::String configFileName_cpp = Ogre::String((const char*) configFileName_c);
  Ogre::String logFileName_cpp = Ogre::String((const char*) logFileName_c);
  Ogre::Root * result_cpp;
  result_cpp = (new Ogre::Root(pluginFileName_cpp, configFileName_cpp, logFileName_cpp));
  *result_c = getHG3DClass_Root((void *) result_cpp);
;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_rt_destruct(struct hg3dclass_struct * thisclass_c)
{
  Ogre::Root * thisclass_cpp = static_cast<Ogre::Root*> (getHG3DClassPtr(*thisclass_c, "Ogre::Root"));
  (delete thisclass_cpp);
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_rt_saveConfig(struct hg3dclass_struct * thisclass_c)
{
  Ogre::Root * thisclass_cpp = static_cast<Ogre::Root*> (getHG3DClassPtr(*thisclass_c, "Ogre::Root"));
  (thisclass_cpp->saveConfig());
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_rt_restoreConfig(struct hg3dclass_struct * thisclass_c, long * result_c)
{
  Ogre::Root * thisclass_cpp = static_cast<Ogre::Root*> (getHG3DClassPtr(*thisclass_c, "Ogre::Root"));
  bool result_cpp;
  result_cpp = (thisclass_cpp->restoreConfig());
  *result_c = (long)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_rt_showConfigDialog(struct hg3dclass_struct * thisclass_c, long * result_c)
{
  Ogre::Root * thisclass_cpp = static_cast<Ogre::Root*> (getHG3DClassPtr(*thisclass_c, "Ogre::Root"));
  bool result_cpp;
  result_cpp = (thisclass_cpp->showConfigDialog());
  *result_c = (long)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_rt_addRenderSystem(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * newRend_c)
{
  Ogre::Root * thisclass_cpp = static_cast<Ogre::Root*> (getHG3DClassPtr(*thisclass_c, "Ogre::Root"));
  Ogre::RenderSystem * newRend_cpp = static_cast<Ogre::RenderSystem*> (getHG3DClassPtr(*newRend_c, "Ogre::RenderSystem"));
  (thisclass_cpp->addRenderSystem(newRend_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_rt_getRenderSystemByName(struct hg3dclass_struct * thisclass_c, char * name_c, struct hg3dclass_struct * result_c)
{
  Ogre::Root * thisclass_cpp = static_cast<Ogre::Root*> (getHG3DClassPtr(*thisclass_c, "Ogre::Root"));
  Ogre::String name_cpp = Ogre::String((const char*) name_c);
  Ogre::RenderSystem * result_cpp;
  result_cpp = (thisclass_cpp->getRenderSystemByName(name_cpp));
  *result_c = getHG3DClass_RenderSystem((void *) result_cpp);
;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_rt_setRenderSystem(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * system_c)
{
  Ogre::Root * thisclass_cpp = static_cast<Ogre::Root*> (getHG3DClassPtr(*thisclass_c, "Ogre::Root"));
  Ogre::RenderSystem * system_cpp = static_cast<Ogre::RenderSystem*> (getHG3DClassPtr(*system_c, "Ogre::RenderSystem"));
  (thisclass_cpp->setRenderSystem(system_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_rt_getRenderSystem(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * result_c)
{
  Ogre::Root * thisclass_cpp = static_cast<Ogre::Root*> (getHG3DClassPtr(*thisclass_c, "Ogre::Root"));
  Ogre::RenderSystem * result_cpp;
  result_cpp = (thisclass_cpp->getRenderSystem());
  *result_c = getHG3DClass_RenderSystem((void *) result_cpp);
;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_rt_initialise(struct hg3dclass_struct * thisclass_c, long autoCreateWindow_c, char * windowTitle_c, char * customCapabilitiesConfig_c, struct hg3dclass_struct * result_c)
{
  Ogre::Root * thisclass_cpp = static_cast<Ogre::Root*> (getHG3DClassPtr(*thisclass_c, "Ogre::Root"));
  bool autoCreateWindow_cpp = (bool)autoCreateWindow_c;
  Ogre::String windowTitle_cpp = Ogre::String((const char*) windowTitle_c);
  Ogre::String customCapabilitiesConfig_cpp = Ogre::String((const char*) customCapabilitiesConfig_c);
  Ogre::RenderWindow * result_cpp;
  result_cpp = (thisclass_cpp->initialise(autoCreateWindow_cpp, windowTitle_cpp, customCapabilitiesConfig_cpp));
  *result_c = getHG3DClass_RenderWindow((void *) result_cpp);
;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_rt_isInitialised(struct hg3dclass_struct * thisclass_c, long * result_c)
{
  Ogre::Root * thisclass_cpp = static_cast<Ogre::Root*> (getHG3DClassPtr(*thisclass_c, "Ogre::Root"));
  bool result_cpp;
  result_cpp = (thisclass_cpp->isInitialised());
  *result_c = (long)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_rt_getRemoveRenderQueueStructuresOnClear(struct hg3dclass_struct * thisclass_c, long * result_c)
{
  Ogre::Root * thisclass_cpp = static_cast<Ogre::Root*> (getHG3DClassPtr(*thisclass_c, "Ogre::Root"));
  bool result_cpp;
  result_cpp = (thisclass_cpp->getRemoveRenderQueueStructuresOnClear());
  *result_c = (long)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_rt_setRemoveRenderQueueStructuresOnClear(struct hg3dclass_struct * thisclass_c, long r_c)
{
  Ogre::Root * thisclass_cpp = static_cast<Ogre::Root*> (getHG3DClassPtr(*thisclass_c, "Ogre::Root"));
  bool r_cpp = (bool)r_c;
  (thisclass_cpp->setRemoveRenderQueueStructuresOnClear(r_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_rt_addSceneManagerFactory(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * fact_c)
{
  Ogre::Root * thisclass_cpp = static_cast<Ogre::Root*> (getHG3DClassPtr(*thisclass_c, "Ogre::Root"));
  Ogre::SceneManagerFactory * fact_cpp = static_cast<Ogre::SceneManagerFactory*> (getHG3DClassPtr(*fact_c, "Ogre::SceneManagerFactory"));
  (thisclass_cpp->addSceneManagerFactory(fact_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_rt_removeSceneManagerFactory(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * fact_c)
{
  Ogre::Root * thisclass_cpp = static_cast<Ogre::Root*> (getHG3DClassPtr(*thisclass_c, "Ogre::Root"));
  Ogre::SceneManagerFactory * fact_cpp = static_cast<Ogre::SceneManagerFactory*> (getHG3DClassPtr(*fact_c, "Ogre::SceneManagerFactory"));
  (thisclass_cpp->removeSceneManagerFactory(fact_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_rt_createSceneManager(struct hg3dclass_struct * thisclass_c, char * typeName_c, char * instanceName_c, struct hg3dclass_struct * result_c)
{
  Ogre::Root * thisclass_cpp = static_cast<Ogre::Root*> (getHG3DClassPtr(*thisclass_c, "Ogre::Root"));
  Ogre::String typeName_cpp = Ogre::String((const char*) typeName_c);
  Ogre::String instanceName_cpp = Ogre::String((const char*) instanceName_c);
  Ogre::SceneManager * result_cpp;
  result_cpp = (thisclass_cpp->createSceneManager(typeName_cpp, instanceName_cpp));
  *result_c = getHG3DClass_SceneManager((void *) result_cpp);
;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_rt_destroySceneManager(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * sm_c)
{
  Ogre::Root * thisclass_cpp = static_cast<Ogre::Root*> (getHG3DClassPtr(*thisclass_c, "Ogre::Root"));
  Ogre::SceneManager * sm_cpp = static_cast<Ogre::SceneManager*> (getHG3DClassPtr(*sm_c, "Ogre::SceneManager"));
  (thisclass_cpp->destroySceneManager(sm_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_rt_getSceneManager(struct hg3dclass_struct * thisclass_c, char * instanceName_c, struct hg3dclass_struct * result_c)
{
  Ogre::Root * thisclass_cpp = static_cast<Ogre::Root*> (getHG3DClassPtr(*thisclass_c, "Ogre::Root"));
  Ogre::String instanceName_cpp = Ogre::String((const char*) instanceName_c);
  Ogre::SceneManager * result_cpp;
  result_cpp = (thisclass_cpp->getSceneManager(instanceName_cpp));
  *result_c = getHG3DClass_SceneManager((void *) result_cpp);
;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_rt_hasSceneManager(struct hg3dclass_struct * thisclass_c, char * instanceName_c, long * result_c)
{
  Ogre::Root * thisclass_cpp = static_cast<Ogre::Root*> (getHG3DClassPtr(*thisclass_c, "Ogre::Root"));
  Ogre::String instanceName_cpp = Ogre::String((const char*) instanceName_c);
  bool result_cpp;
  result_cpp = (thisclass_cpp->hasSceneManager(instanceName_cpp));
  *result_c = (long)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_rt_getTextureManager(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * result_c)
{
  Ogre::Root * thisclass_cpp = static_cast<Ogre::Root*> (getHG3DClassPtr(*thisclass_c, "Ogre::Root"));
  Ogre::TextureManager * result_cpp;
  result_cpp = (thisclass_cpp->getTextureManager());
  *result_c = getHG3DClass_TextureManager((void *) result_cpp);
;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_rt_getMeshManager(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * result_c)
{
  Ogre::Root * thisclass_cpp = static_cast<Ogre::Root*> (getHG3DClassPtr(*thisclass_c, "Ogre::Root"));
  Ogre::MeshManager * result_cpp;
  result_cpp = (thisclass_cpp->getMeshManager());
  *result_c = getHG3DClass_MeshManager((void *) result_cpp);
;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_rt_getErrorDescription(struct hg3dclass_struct * thisclass_c, long errorNumber_c, char * result_c)
{
  Ogre::Root * thisclass_cpp = static_cast<Ogre::Root*> (getHG3DClassPtr(*thisclass_c, "Ogre::Root"));
  long errorNumber_cpp = (long)errorNumber_c;
  Ogre::String result_cpp;
  result_cpp = (thisclass_cpp->getErrorDescription(errorNumber_cpp));
  if (strlen( (char *) result_cpp.c_str()) < (1024 * 64 - 1))  { 
    strcpy(result_c, (char *) result_cpp.c_str()); 
  } else {
    strcpy(result_c, "error: outstring larger then 64k");
  };
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_rt_queueEndRendering(struct hg3dclass_struct * thisclass_c)
{
  Ogre::Root * thisclass_cpp = static_cast<Ogre::Root*> (getHG3DClassPtr(*thisclass_c, "Ogre::Root"));
  (thisclass_cpp->queueEndRendering());
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_rt_startRendering(struct hg3dclass_struct * thisclass_c)
{
  Ogre::Root * thisclass_cpp = static_cast<Ogre::Root*> (getHG3DClassPtr(*thisclass_c, "Ogre::Root"));
  (thisclass_cpp->startRendering());
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_rt_renderOneFrame(struct hg3dclass_struct * thisclass_c, long * result_c)
{
  Ogre::Root * thisclass_cpp = static_cast<Ogre::Root*> (getHG3DClassPtr(*thisclass_c, "Ogre::Root"));
  bool result_cpp;
  result_cpp = (thisclass_cpp->renderOneFrame());
  *result_c = (long)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_rt_renderOneFrame2(struct hg3dclass_struct * thisclass_c, float timeSinceLastFrame_c, long * result_c)
{
  Ogre::Root * thisclass_cpp = static_cast<Ogre::Root*> (getHG3DClassPtr(*thisclass_c, "Ogre::Root"));
  Real timeSinceLastFrame_cpp = (Real)timeSinceLastFrame_c;
  bool result_cpp;
  result_cpp = (thisclass_cpp->renderOneFrame(timeSinceLastFrame_cpp));
  *result_c = (long)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_rt_shutdown(struct hg3dclass_struct * thisclass_c)
{
  Ogre::Root * thisclass_cpp = static_cast<Ogre::Root*> (getHG3DClassPtr(*thisclass_c, "Ogre::Root"));
  (thisclass_cpp->shutdown());
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_rt_addResourceLocation(struct hg3dclass_struct * thisclass_c, char * name_c, char * locType_c, char * groupName_c, long recursive_c)
{
  Ogre::Root * thisclass_cpp = static_cast<Ogre::Root*> (getHG3DClassPtr(*thisclass_c, "Ogre::Root"));
  Ogre::String name_cpp = Ogre::String((const char*) name_c);
  Ogre::String locType_cpp = Ogre::String((const char*) locType_c);
  Ogre::String groupName_cpp = Ogre::String((const char*) groupName_c);
  bool recursive_cpp = (bool)recursive_c;
  (thisclass_cpp->addResourceLocation(name_cpp, locType_cpp, groupName_cpp, recursive_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_rt_removeResourceLocation(struct hg3dclass_struct * thisclass_c, char * name_c, char * groupName_c)
{
  Ogre::Root * thisclass_cpp = static_cast<Ogre::Root*> (getHG3DClassPtr(*thisclass_c, "Ogre::Root"));
  Ogre::String name_cpp = Ogre::String((const char*) name_c);
  Ogre::String groupName_cpp = Ogre::String((const char*) groupName_c);
  (thisclass_cpp->removeResourceLocation(name_cpp, groupName_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_rt_convertColourValue(struct hg3dclass_struct * thisclass_c, struct colourvalue_struct * colour_c, unsigned long * pDest_c)
{
  Ogre::Root * thisclass_cpp = static_cast<Ogre::Root*> (getHG3DClassPtr(*thisclass_c, "Ogre::Root"));
  ColourValue colour_cpp = *((ColourValue*) colour_c);
  uint32 pDest_cpp;
  (thisclass_cpp->convertColourValue(colour_cpp, &pDest_cpp));
  *pDest_c = (unsigned long)pDest_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_rt_getAutoCreatedWindow(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * result_c)
{
  Ogre::Root * thisclass_cpp = static_cast<Ogre::Root*> (getHG3DClassPtr(*thisclass_c, "Ogre::Root"));
  Ogre::RenderWindow * result_cpp;
  result_cpp = (thisclass_cpp->getAutoCreatedWindow());
  *result_c = getHG3DClass_RenderWindow((void *) result_cpp);
;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_rt_detachRenderTarget(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * pWin_c, struct hg3dclass_struct * result_c)
{
  Ogre::Root * thisclass_cpp = static_cast<Ogre::Root*> (getHG3DClassPtr(*thisclass_c, "Ogre::Root"));
  Ogre::RenderTarget * pWin_cpp = static_cast<Ogre::RenderTarget*> (getHG3DClassPtr(*pWin_c, "Ogre::RenderTarget"));
  Ogre::RenderTarget * result_cpp;
  result_cpp = (thisclass_cpp->detachRenderTarget(pWin_cpp));
  *result_c = getHG3DClass_RenderTarget((void *) result_cpp);
;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_rt_detachRenderTarget2(struct hg3dclass_struct * thisclass_c, char * name_c, struct hg3dclass_struct * result_c)
{
  Ogre::Root * thisclass_cpp = static_cast<Ogre::Root*> (getHG3DClassPtr(*thisclass_c, "Ogre::Root"));
  Ogre::String name_cpp = Ogre::String((const char*) name_c);
  Ogre::RenderTarget * result_cpp;
  result_cpp = (thisclass_cpp->detachRenderTarget(name_cpp));
  *result_c = getHG3DClass_RenderTarget((void *) result_cpp);
;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_rt_destroyRenderTarget(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * target_c)
{
  Ogre::Root * thisclass_cpp = static_cast<Ogre::Root*> (getHG3DClassPtr(*thisclass_c, "Ogre::Root"));
  Ogre::RenderTarget * target_cpp = static_cast<Ogre::RenderTarget*> (getHG3DClassPtr(*target_c, "Ogre::RenderTarget"));
  (thisclass_cpp->destroyRenderTarget(target_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_rt_destroyRenderTarget2(struct hg3dclass_struct * thisclass_c, char * name_c)
{
  Ogre::Root * thisclass_cpp = static_cast<Ogre::Root*> (getHG3DClassPtr(*thisclass_c, "Ogre::Root"));
  Ogre::String name_cpp = Ogre::String((const char*) name_c);
  (thisclass_cpp->destroyRenderTarget(name_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_rt_getRenderTarget(struct hg3dclass_struct * thisclass_c, char * name_c, struct hg3dclass_struct * result_c)
{
  Ogre::Root * thisclass_cpp = static_cast<Ogre::Root*> (getHG3DClassPtr(*thisclass_c, "Ogre::Root"));
  Ogre::String name_cpp = Ogre::String((const char*) name_c);
  Ogre::RenderTarget * result_cpp;
  result_cpp = (thisclass_cpp->getRenderTarget(name_cpp));
  *result_c = getHG3DClass_RenderTarget((void *) result_cpp);
;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_rt_loadPlugin(struct hg3dclass_struct * thisclass_c, char * pluginName_c)
{
  Ogre::Root * thisclass_cpp = static_cast<Ogre::Root*> (getHG3DClassPtr(*thisclass_c, "Ogre::Root"));
  Ogre::String pluginName_cpp = Ogre::String((const char*) pluginName_c);
  (thisclass_cpp->loadPlugin(pluginName_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_rt_unloadPlugin(struct hg3dclass_struct * thisclass_c, char * pluginName_c)
{
  Ogre::Root * thisclass_cpp = static_cast<Ogre::Root*> (getHG3DClassPtr(*thisclass_c, "Ogre::Root"));
  Ogre::String pluginName_cpp = Ogre::String((const char*) pluginName_c);
  (thisclass_cpp->unloadPlugin(pluginName_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_rt_getNextFrameNumber(struct hg3dclass_struct * thisclass_c, unsigned long * result_c)
{
  Ogre::Root * thisclass_cpp = static_cast<Ogre::Root*> (getHG3DClassPtr(*thisclass_c, "Ogre::Root"));
  unsigned long result_cpp;
  result_cpp = (thisclass_cpp->getNextFrameNumber());
  *result_c = (unsigned long)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_rt_destroyRenderQueueInvocationSequence(struct hg3dclass_struct * thisclass_c, char * name_c)
{
  Ogre::Root * thisclass_cpp = static_cast<Ogre::Root*> (getHG3DClassPtr(*thisclass_c, "Ogre::Root"));
  Ogre::String name_cpp = Ogre::String((const char*) name_c);
  (thisclass_cpp->destroyRenderQueueInvocationSequence(name_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_rt_destroyAllRenderQueueInvocationSequences(struct hg3dclass_struct * thisclass_c)
{
  Ogre::Root * thisclass_cpp = static_cast<Ogre::Root*> (getHG3DClassPtr(*thisclass_c, "Ogre::Root"));
  (thisclass_cpp->destroyAllRenderQueueInvocationSequences());
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_rt_clearEventTimes(struct hg3dclass_struct * thisclass_c)
{
  Ogre::Root * thisclass_cpp = static_cast<Ogre::Root*> (getHG3DClassPtr(*thisclass_c, "Ogre::Root"));
  (thisclass_cpp->clearEventTimes());
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_rt_setFrameSmoothingPeriod(struct hg3dclass_struct * thisclass_c, float period_c)
{
  Ogre::Root * thisclass_cpp = static_cast<Ogre::Root*> (getHG3DClassPtr(*thisclass_c, "Ogre::Root"));
  Real period_cpp = (Real)period_c;
  (thisclass_cpp->setFrameSmoothingPeriod(period_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_rt_getFrameSmoothingPeriod(struct hg3dclass_struct * thisclass_c, float * result_c)
{
  Ogre::Root * thisclass_cpp = static_cast<Ogre::Root*> (getHG3DClassPtr(*thisclass_c, "Ogre::Root"));
  Real result_cpp;
  result_cpp = (thisclass_cpp->getFrameSmoothingPeriod());
  *result_c = (float)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_rt_addMovableObjectFactory(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * fact_c, long overrideExisting_c)
{
  Ogre::Root * thisclass_cpp = static_cast<Ogre::Root*> (getHG3DClassPtr(*thisclass_c, "Ogre::Root"));
  Ogre::MovableObjectFactory * fact_cpp = static_cast<Ogre::MovableObjectFactory*> (getHG3DClassPtr(*fact_c, "Ogre::MovableObjectFactory"));
  bool overrideExisting_cpp = (bool)overrideExisting_c;
  (thisclass_cpp->addMovableObjectFactory(fact_cpp, overrideExisting_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_rt_removeMovableObjectFactory(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * fact_c)
{
  Ogre::Root * thisclass_cpp = static_cast<Ogre::Root*> (getHG3DClassPtr(*thisclass_c, "Ogre::Root"));
  Ogre::MovableObjectFactory * fact_cpp = static_cast<Ogre::MovableObjectFactory*> (getHG3DClassPtr(*fact_c, "Ogre::MovableObjectFactory"));
  (thisclass_cpp->removeMovableObjectFactory(fact_cpp));
};

// Checks whether a factory is registered for a given MovableObject
extern "C" Ogre_LIB_EXPORT void ogre_rt_hasMovableObjectFactory(struct hg3dclass_struct * thisclass_c, char * typeName_c, long * result_c)
{
  Ogre::Root * thisclass_cpp = static_cast<Ogre::Root*> (getHG3DClassPtr(*thisclass_c, "Ogre::Root"));
  Ogre::String typeName_cpp = Ogre::String((const char*) typeName_c);
  bool result_cpp;
  result_cpp = (thisclass_cpp->hasMovableObjectFactory(typeName_cpp));
  *result_c = (long)result_cpp;
};

// Get a MovableObjectFactory
extern "C" Ogre_LIB_EXPORT void ogre_rt_getMovableObjectFactory(struct hg3dclass_struct * thisclass_c, char * typeName_c, struct hg3dclass_struct * result_c)
{
  Ogre::Root * thisclass_cpp = static_cast<Ogre::Root*> (getHG3DClassPtr(*thisclass_c, "Ogre::Root"));
  Ogre::String typeName_cpp = Ogre::String((const char*) typeName_c);
  Ogre::MovableObjectFactory * result_cpp;
  result_cpp = (thisclass_cpp->getMovableObjectFactory(typeName_cpp));
  *result_c = getHG3DClass_MovableObjectFactory((void *) result_cpp);
;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_rt_getDisplayMonitorCount(struct hg3dclass_struct * thisclass_c, unsigned long * result_c)
{
  Ogre::Root * thisclass_cpp = static_cast<Ogre::Root*> (getHG3DClassPtr(*thisclass_c, "Ogre::Root"));
  unsigned int result_cpp;
  result_cpp = (thisclass_cpp->getDisplayMonitorCount());
  *result_c = (unsigned long)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_rt_setBlendIndicesGpuRedundant(struct hg3dclass_struct * thisclass_c, long redundant_c)
{
  Ogre::Root * thisclass_cpp = static_cast<Ogre::Root*> (getHG3DClassPtr(*thisclass_c, "Ogre::Root"));
  bool redundant_cpp = (bool)redundant_c;
  (thisclass_cpp->setBlendIndicesGpuRedundant(redundant_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_rt_isBlendIndicesGpuRedundant(struct hg3dclass_struct * thisclass_c, long * result_c)
{
  Ogre::Root * thisclass_cpp = static_cast<Ogre::Root*> (getHG3DClassPtr(*thisclass_c, "Ogre::Root"));
  bool result_cpp;
  result_cpp = (thisclass_cpp->isBlendIndicesGpuRedundant());
  *result_c = (long)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_rt_setBlendWeightsGpuRedundant(struct hg3dclass_struct * thisclass_c, long redundant_c)
{
  Ogre::Root * thisclass_cpp = static_cast<Ogre::Root*> (getHG3DClassPtr(*thisclass_c, "Ogre::Root"));
  bool redundant_cpp = (bool)redundant_c;
  (thisclass_cpp->setBlendWeightsGpuRedundant(redundant_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_rt_isBlendWeightsGpuRedundant(struct hg3dclass_struct * thisclass_c, long * result_c)
{
  Ogre::Root * thisclass_cpp = static_cast<Ogre::Root*> (getHG3DClassPtr(*thisclass_c, "Ogre::Root"));
  bool result_cpp;
  result_cpp = (thisclass_cpp->isBlendWeightsGpuRedundant());
  *result_c = (long)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_rt_setDefaultMinPixelSize(struct hg3dclass_struct * thisclass_c, float pixelSize_c)
{
  Ogre::Root * thisclass_cpp = static_cast<Ogre::Root*> (getHG3DClassPtr(*thisclass_c, "Ogre::Root"));
  Real pixelSize_cpp = (Real)pixelSize_c;
  (thisclass_cpp->setDefaultMinPixelSize(pixelSize_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_rt_getDefaultMinPixelSize(struct hg3dclass_struct * thisclass_c, float * result_c)
{
  Ogre::Root * thisclass_cpp = static_cast<Ogre::Root*> (getHG3DClassPtr(*thisclass_c, "Ogre::Root"));
  Real result_cpp;
  result_cpp = (thisclass_cpp->getDefaultMinPixelSize());
  *result_c = (float)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_rt_getSingleton(struct hg3dclass_struct * result_c)
{
  Ogre::Root * result_cpp;
  result_cpp = &(Ogre::Root::getSingleton());
  *result_c = getHG3DClass_Root((void *) result_cpp);
;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_rt_getSingletonPtr(struct hg3dclass_struct * result_c)
{
  Ogre::Root * result_cpp;
  result_cpp = (Ogre::Root::getSingletonPtr());
  *result_c = getHG3DClass_Root((void *) result_cpp);
;
};

