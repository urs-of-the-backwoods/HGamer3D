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

// ClassOgreRenderer.h

// 

#include "wchar.h"
#ifndef _DEFINED_HG3D_ClassOgreRenderer
#define _DEFINED_HG3D_ClassOgreRenderer

#include "ClassPtr.h"
#include "ClassOgreResourceProvider.h"
#include "EnumBlendMode.h"


// Convenience function that creates all the OgreCEGUI
void cegui_ogrrndr_bootstrapSystem(struct hg3dclass_struct * result_c);

// Convenience function to cleanup the CEGUI
void cegui_ogrrndr_destroySystem();

// Create an OgreRendererOgre
void cegui_ogrrndr_create(struct hg3dclass_struct * result_c);

// destory an OgreRenderer
void cegui_ogrrndr_destroy(struct hg3dclass_struct * renderer_c);

// function to create a CEGUI::OgreResourceProvider
void cegui_ogrrndr_createOgreResourceProvider(struct hg3dclass_struct * result_c);

// function to destroy a CEGUI::OgreResourceProvider
void cegui_ogrrndr_destroyOgreResourceProvider(struct hg3dclass_struct * rp_c);

// set whether CEGUI
void cegui_ogrrndr_setRenderingEnabled(struct hg3dclass_struct * thisclass_c, const int enabled_c);

// return whether CEGUI
void cegui_ogrrndr_isRenderingEnabled(struct hg3dclass_struct * thisclass_c, int * result_c);

// set the render states for the specified BlendMode. 
void cegui_ogrrndr_setupRenderingBlendMode(struct hg3dclass_struct * thisclass_c, const enum EnumBlendMode mode_c, const int force_c);

// Controls whether rendering done by CEGUI
void cegui_ogrrndr_setFrameControlExecutionEnabled(struct hg3dclass_struct * thisclass_c, const int enabled_c);

// Returns whether rendering done by CEGUI
void cegui_ogrrndr_isFrameControlExecutionEnabled(struct hg3dclass_struct * thisclass_c, int * result_c);

// Sets all the required render states needed for CEGUI
void cegui_ogrrndr_initialiseRenderStateSettings(struct hg3dclass_struct * thisclass_c);

// Destroy all GeometryBuffer objects created by this Renderer
void cegui_ogrrndr_destroyAllGeometryBuffers(struct hg3dclass_struct * thisclass_c);

// Destory all TextureTarget objects created by this Renderer
void cegui_ogrrndr_destroyAllTextureTargets(struct hg3dclass_struct * thisclass_c);

// Destroy all Texture objects created by this Renderer
void cegui_ogrrndr_destroyAllTextures(struct hg3dclass_struct * thisclass_c);

// Perform any operations required to put the system into a state ready for rendering operations to begin. 
void cegui_ogrrndr_beginRendering(struct hg3dclass_struct * thisclass_c);

// Perform any operations required to finalise rendering. 
void cegui_ogrrndr_endRendering(struct hg3dclass_struct * thisclass_c);

// Return the pixel size of the maximum supported texture. 
void cegui_ogrrndr_getMaxTextureSize(struct hg3dclass_struct * thisclass_c, unsigned int * result_c);

// Return identification string for the renderer module. 
void cegui_ogrrndr_getIdentifierString(struct hg3dclass_struct * thisclass_c, char * result_c);

#endif 
