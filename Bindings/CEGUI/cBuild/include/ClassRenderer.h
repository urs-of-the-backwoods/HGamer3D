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

// ClassRenderer.h

// 

#include "wchar.h"
#ifndef _DEFINED_HG3D_ClassRenderer
#define _DEFINED_HG3D_ClassRenderer

#include "ClassPtr.h"


// Destroy all GeometryBuffer objects created by this Renderer
void cegui_rndr_destroyAllGeometryBuffers(struct hg3dclass_struct * thisclass_c);

// Destory all TextureTarget objects created by this Renderer
void cegui_rndr_destroyAllTextureTargets(struct hg3dclass_struct * thisclass_c);

// Destroy all Texture objects created by this Renderer
void cegui_rndr_destroyAllTextures(struct hg3dclass_struct * thisclass_c);

// Perform any operations required to put the system into a state ready for rendering operations to begin. 
void cegui_rndr_beginRendering(struct hg3dclass_struct * thisclass_c);

// Perform any operations required to finalise rendering. 
void cegui_rndr_endRendering(struct hg3dclass_struct * thisclass_c);

// Return the pixel size of the maximum supported texture. 
void cegui_rndr_getMaxTextureSize(struct hg3dclass_struct * thisclass_c, unsigned int * result_c);

// Return identification string for the renderer module. 
void cegui_rndr_getIdentifierString(struct hg3dclass_struct * thisclass_c, char * result_c);

// Destructor. 
void cegui_rndr_destruct(struct hg3dclass_struct * thisclass_c);

#endif 
