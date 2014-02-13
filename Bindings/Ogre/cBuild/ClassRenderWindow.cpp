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

// ClassRenderWindow.cpp

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



// 
extern "C" Ogre_LIB_EXPORT void ogre_rw_setFullscreen(struct hg3dclass_struct * thisclass_c, int fullScreen_c, unsigned int width_c, unsigned int height_c)
{
  Ogre::RenderWindow * thisclass_cpp = static_cast<Ogre::RenderWindow*> (getHG3DClassPtr(*thisclass_c, "Ogre::RenderWindow"));
  bool fullScreen_cpp = (bool)fullScreen_c;
  unsigned int width_cpp = (unsigned int)width_c;
  unsigned int height_cpp = (unsigned int)height_c;
  (thisclass_cpp->setFullscreen(fullScreen_cpp, width_cpp, height_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_rw_destroy(struct hg3dclass_struct * thisclass_c)
{
  Ogre::RenderWindow * thisclass_cpp = static_cast<Ogre::RenderWindow*> (getHG3DClassPtr(*thisclass_c, "Ogre::RenderWindow"));
  (thisclass_cpp->destroy());
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_rw_resize(struct hg3dclass_struct * thisclass_c, unsigned int width_c, unsigned int height_c)
{
  Ogre::RenderWindow * thisclass_cpp = static_cast<Ogre::RenderWindow*> (getHG3DClassPtr(*thisclass_c, "Ogre::RenderWindow"));
  unsigned int width_cpp = (unsigned int)width_c;
  unsigned int height_cpp = (unsigned int)height_c;
  (thisclass_cpp->resize(width_cpp, height_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_rw_windowMovedOrResized(struct hg3dclass_struct * thisclass_c)
{
  Ogre::RenderWindow * thisclass_cpp = static_cast<Ogre::RenderWindow*> (getHG3DClassPtr(*thisclass_c, "Ogre::RenderWindow"));
  (thisclass_cpp->windowMovedOrResized());
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_rw_reposition(struct hg3dclass_struct * thisclass_c, int left_c, int top_c)
{
  Ogre::RenderWindow * thisclass_cpp = static_cast<Ogre::RenderWindow*> (getHG3DClassPtr(*thisclass_c, "Ogre::RenderWindow"));
  int left_cpp = (int)left_c;
  int top_cpp = (int)top_c;
  (thisclass_cpp->reposition(left_cpp, top_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_rw_isVisible(struct hg3dclass_struct * thisclass_c, int * result_c)
{
  Ogre::RenderWindow * thisclass_cpp = static_cast<Ogre::RenderWindow*> (getHG3DClassPtr(*thisclass_c, "Ogre::RenderWindow"));
  bool result_cpp;
  result_cpp = (thisclass_cpp->isVisible());
  *result_c = (int)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_rw_setVisible(struct hg3dclass_struct * thisclass_c, int visible_c)
{
  Ogre::RenderWindow * thisclass_cpp = static_cast<Ogre::RenderWindow*> (getHG3DClassPtr(*thisclass_c, "Ogre::RenderWindow"));
  bool visible_cpp = (bool)visible_c;
  (thisclass_cpp->setVisible(visible_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_rw_isHidden(struct hg3dclass_struct * thisclass_c, int * result_c)
{
  Ogre::RenderWindow * thisclass_cpp = static_cast<Ogre::RenderWindow*> (getHG3DClassPtr(*thisclass_c, "Ogre::RenderWindow"));
  bool result_cpp;
  result_cpp = (thisclass_cpp->isHidden());
  *result_c = (int)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_rw_setHidden(struct hg3dclass_struct * thisclass_c, int hidden_c)
{
  Ogre::RenderWindow * thisclass_cpp = static_cast<Ogre::RenderWindow*> (getHG3DClassPtr(*thisclass_c, "Ogre::RenderWindow"));
  bool hidden_cpp = (bool)hidden_c;
  (thisclass_cpp->setHidden(hidden_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_rw_setVSyncEnabled(struct hg3dclass_struct * thisclass_c, int vsync_c)
{
  Ogre::RenderWindow * thisclass_cpp = static_cast<Ogre::RenderWindow*> (getHG3DClassPtr(*thisclass_c, "Ogre::RenderWindow"));
  bool vsync_cpp = (bool)vsync_c;
  (thisclass_cpp->setVSyncEnabled(vsync_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_rw_isVSyncEnabled(struct hg3dclass_struct * thisclass_c, int * result_c)
{
  Ogre::RenderWindow * thisclass_cpp = static_cast<Ogre::RenderWindow*> (getHG3DClassPtr(*thisclass_c, "Ogre::RenderWindow"));
  bool result_cpp;
  result_cpp = (thisclass_cpp->isVSyncEnabled());
  *result_c = (int)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_rw_setVSyncInterval(struct hg3dclass_struct * thisclass_c, unsigned int interval_c)
{
  Ogre::RenderWindow * thisclass_cpp = static_cast<Ogre::RenderWindow*> (getHG3DClassPtr(*thisclass_c, "Ogre::RenderWindow"));
  unsigned int interval_cpp = (unsigned int)interval_c;
  (thisclass_cpp->setVSyncInterval(interval_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_rw_getVSyncInterval(struct hg3dclass_struct * thisclass_c, unsigned int * result_c)
{
  Ogre::RenderWindow * thisclass_cpp = static_cast<Ogre::RenderWindow*> (getHG3DClassPtr(*thisclass_c, "Ogre::RenderWindow"));
  unsigned int result_cpp;
  result_cpp = (thisclass_cpp->getVSyncInterval());
  *result_c = (unsigned int)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_rw_isActive(struct hg3dclass_struct * thisclass_c, int * result_c)
{
  Ogre::RenderWindow * thisclass_cpp = static_cast<Ogre::RenderWindow*> (getHG3DClassPtr(*thisclass_c, "Ogre::RenderWindow"));
  bool result_cpp;
  result_cpp = (thisclass_cpp->isActive());
  *result_c = (int)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_rw_isClosed(struct hg3dclass_struct * thisclass_c, int * result_c)
{
  Ogre::RenderWindow * thisclass_cpp = static_cast<Ogre::RenderWindow*> (getHG3DClassPtr(*thisclass_c, "Ogre::RenderWindow"));
  bool result_cpp;
  result_cpp = (thisclass_cpp->isClosed());
  *result_c = (int)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_rw_isPrimary(struct hg3dclass_struct * thisclass_c, int * result_c)
{
  Ogre::RenderWindow * thisclass_cpp = static_cast<Ogre::RenderWindow*> (getHG3DClassPtr(*thisclass_c, "Ogre::RenderWindow"));
  bool result_cpp;
  result_cpp = (thisclass_cpp->isPrimary());
  *result_c = (int)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_rw_isFullScreen(struct hg3dclass_struct * thisclass_c, int * result_c)
{
  Ogre::RenderWindow * thisclass_cpp = static_cast<Ogre::RenderWindow*> (getHG3DClassPtr(*thisclass_c, "Ogre::RenderWindow"));
  bool result_cpp;
  result_cpp = (thisclass_cpp->isFullScreen());
  *result_c = (int)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_rw_getMetrics(struct hg3dclass_struct * thisclass_c, unsigned int * width_c, unsigned int * height_c, unsigned int * colourDepth_c, int * left_c, int * top_c)
{
  Ogre::RenderWindow * thisclass_cpp = static_cast<Ogre::RenderWindow*> (getHG3DClassPtr(*thisclass_c, "Ogre::RenderWindow"));
  unsigned int width_cpp;
  unsigned int height_cpp;
  unsigned int colourDepth_cpp;
  int left_cpp;
  int top_cpp;
  (thisclass_cpp->getMetrics(width_cpp, height_cpp, colourDepth_cpp, left_cpp, top_cpp));
  *width_c = (unsigned int)width_cpp;
  *height_c = (unsigned int)height_cpp;
  *colourDepth_c = (unsigned int)colourDepth_cpp;
  *left_c = (int)left_cpp;
  *top_c = (int)top_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_rw_isDeactivatedOnFocusChange(struct hg3dclass_struct * thisclass_c, int * result_c)
{
  Ogre::RenderWindow * thisclass_cpp = static_cast<Ogre::RenderWindow*> (getHG3DClassPtr(*thisclass_c, "Ogre::RenderWindow"));
  bool result_cpp;
  result_cpp = (thisclass_cpp->isDeactivatedOnFocusChange());
  *result_c = (int)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_rw_setDeactivateOnFocusChange(struct hg3dclass_struct * thisclass_c, int deactivate_c)
{
  Ogre::RenderWindow * thisclass_cpp = static_cast<Ogre::RenderWindow*> (getHG3DClassPtr(*thisclass_c, "Ogre::RenderWindow"));
  bool deactivate_cpp = (bool)deactivate_c;
  (thisclass_cpp->setDeactivateOnFocusChange(deactivate_cpp));
};

