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

// ClassControllerManager.h

// 

#include "wchar.h"
#ifndef _DEFINED_HG3D_ClassControllerManager
#define _DEFINED_HG3D_ClassControllerManager

#include "ClassPtr.h"


// 
void ogre_cmgr_construct(struct hg3dclass_struct * result_c);

// 
void ogre_cmgr_destruct(struct hg3dclass_struct * thisclass_c);

// 
void ogre_cmgr_clearControllers(struct hg3dclass_struct * thisclass_c);

// 
void ogre_cmgr_updateAllControllers(struct hg3dclass_struct * thisclass_c);

// 
void ogre_cmgr_getTimeFactor(struct hg3dclass_struct * thisclass_c, float * result_c);

// 
void ogre_cmgr_setTimeFactor(struct hg3dclass_struct * thisclass_c, float tf_c);

// 
void ogre_cmgr_getFrameDelay(struct hg3dclass_struct * thisclass_c, float * result_c);

// 
void ogre_cmgr_setFrameDelay(struct hg3dclass_struct * thisclass_c, float fd_c);

// 
void ogre_cmgr_getElapsedTime(struct hg3dclass_struct * thisclass_c, float * result_c);

// 
void ogre_cmgr_setElapsedTime(struct hg3dclass_struct * thisclass_c, float elapsedTime_c);

// 
void ogre_cmgr_getSingleton(struct hg3dclass_struct * result_c);

// 
void ogre_cmgr_getSingletonPtr(struct hg3dclass_struct * result_c);

#endif 
