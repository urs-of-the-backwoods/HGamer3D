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

// ClassViewport.h

// 

#include "wchar.h"
#ifndef _DEFINED_HG3D_ClassViewport
#define _DEFINED_HG3D_ClassViewport

#include "ClassPtr.h"
#include "ClassCamera.h"
#include "ClassRenderTarget.h"
#include "StructColour.h"
#include "EnumOrientationMode.h"
#include "StructVec2.h"


// 
void ogre_vprt_construct(struct hg3dclass_struct * camera_c, struct hg3dclass_struct * target_c, float left_c, float top_c, float width_c, float height_c, long ZOrder_c, struct hg3dclass_struct * result_c);

// 
void ogre_vprt_destruct(struct hg3dclass_struct * thisclass_c);

// 
void ogre_vprt_update(struct hg3dclass_struct * thisclass_c);

// 
void ogre_vprt_clear(struct hg3dclass_struct * thisclass_c, unsigned long buffers_c, struct colourvalue_struct * colour_c, float depth_c, unsigned short stencil_c);

// 
void ogre_vprt_getTarget(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * result_c);

// 
void ogre_vprt_getCamera(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * result_c);

// 
void ogre_vprt_setCamera(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * cam_c);

// 
void ogre_vprt_getZOrder(struct hg3dclass_struct * thisclass_c, long * result_c);

// 
void ogre_vprt_getLeft(struct hg3dclass_struct * thisclass_c, float * result_c);

// 
void ogre_vprt_getTop(struct hg3dclass_struct * thisclass_c, float * result_c);

// 
void ogre_vprt_getWidth(struct hg3dclass_struct * thisclass_c, float * result_c);

// 
void ogre_vprt_getHeight(struct hg3dclass_struct * thisclass_c, float * result_c);

// 
void ogre_vprt_getActualLeft(struct hg3dclass_struct * thisclass_c, long * result_c);

// 
void ogre_vprt_getActualTop(struct hg3dclass_struct * thisclass_c, long * result_c);

// 
void ogre_vprt_getActualWidth(struct hg3dclass_struct * thisclass_c, long * result_c);

// 
void ogre_vprt_getActualHeight(struct hg3dclass_struct * thisclass_c, long * result_c);

// 
void ogre_vprt_setDimensions(struct hg3dclass_struct * thisclass_c, float left_c, float top_c, float width_c, float height_c);

// 
void ogre_vprt_setOrientationMode(struct hg3dclass_struct * thisclass_c, enum EnumOrientationMode orientationMode_c, long setDefault_c);

// 
void ogre_vprt_getOrientationMode(struct hg3dclass_struct * thisclass_c, enum EnumOrientationMode * result_c);

// 
void ogre_vprt_setBackgroundColour(struct hg3dclass_struct * thisclass_c, struct colourvalue_struct * colour_c);

// 
void ogre_vprt_getBackgroundColour(struct hg3dclass_struct * thisclass_c, struct colourvalue_struct * result_c);

// 
void ogre_vprt_setDepthClear(struct hg3dclass_struct * thisclass_c, float depth_c);

// 
void ogre_vprt_getDepthClear(struct hg3dclass_struct * thisclass_c, float * result_c);

// 
void ogre_vprt_setClearEveryFrame(struct hg3dclass_struct * thisclass_c, long clear_c, unsigned long buffers_c);

// 
void ogre_vprt_getClearEveryFrame(struct hg3dclass_struct * thisclass_c, long * result_c);

// 
void ogre_vprt_getClearBuffers(struct hg3dclass_struct * thisclass_c, unsigned long * result_c);

// 
void ogre_vprt_setAutoUpdated(struct hg3dclass_struct * thisclass_c, long autoupdate_c);

// 
void ogre_vprt_isAutoUpdated(struct hg3dclass_struct * thisclass_c, long * result_c);

// 
void ogre_vprt_setMaterialScheme(struct hg3dclass_struct * thisclass_c, char * schemeName_c);

// 
void ogre_vprt_getMaterialScheme(struct hg3dclass_struct * thisclass_c, char * result_c);

// 
void ogre_vprt_getActualDimensions(struct hg3dclass_struct * thisclass_c, long * left_c, long * top_c, long * width_c, long * height_c);

// 
void ogre_vprt_setOverlaysEnabled(struct hg3dclass_struct * thisclass_c, long enabled_c);

// 
void ogre_vprt_getOverlaysEnabled(struct hg3dclass_struct * thisclass_c, long * result_c);

// 
void ogre_vprt_setSkiesEnabled(struct hg3dclass_struct * thisclass_c, long enabled_c);

// 
void ogre_vprt_getSkiesEnabled(struct hg3dclass_struct * thisclass_c, long * result_c);

// 
void ogre_vprt_setShadowsEnabled(struct hg3dclass_struct * thisclass_c, long enabled_c);

// 
void ogre_vprt_getShadowsEnabled(struct hg3dclass_struct * thisclass_c, long * result_c);

// 
void ogre_vprt_setVisibilityMask(struct hg3dclass_struct * thisclass_c, unsigned long mask_c);

// 
void ogre_vprt_getVisibilityMask(struct hg3dclass_struct * thisclass_c, unsigned long * result_c);

// 
void ogre_vprt_setRenderQueueInvocationSequenceName(struct hg3dclass_struct * thisclass_c, char * sequenceName_c);

// 
void ogre_vprt_getRenderQueueInvocationSequenceName(struct hg3dclass_struct * thisclass_c, char * result_c);

// 
void ogre_vprt_pointOrientedToScreen(struct hg3dclass_struct * thisclass_c, struct vector2_struct * v_c, long orientationMode_c, struct vector2_struct * outv_c);

// 
void ogre_vprt_pointOrientedToScreen2(struct hg3dclass_struct * thisclass_c, float orientedX_c, float orientedY_c, long orientationMode_c, float * screenX_c, float * screenY_c);

// 
void ogre_vprt_setDefaultOrientationMode(enum EnumOrientationMode orientationMode_c);

// 
void ogre_vprt_getDefaultOrientationMode(enum EnumOrientationMode * result_c);

#endif 
