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

// ClassImageset.h

// 

#include "wchar.h"
#ifndef _DEFINED_HG3D_ClassImageset
#define _DEFINED_HG3D_ClassImageset

#include "ClassPtr.h"


// Construct a new Imageset
void cegui_imgst_construct(char * name_c, char * filename_c, char * resourceGroup_c, struct hg3dclass_struct * result_c);

// Destroys Imageset
void cegui_imgst_destruct(struct hg3dclass_struct * thisclass_c);

// return String object holding the name of the Imageset
void cegui_imgst_getName(struct hg3dclass_struct * thisclass_c, char * result_c);

// return number of images defined for this Imageset
void cegui_imgst_getImageCount(struct hg3dclass_struct * thisclass_c, unsigned int * result_c);

// return true if an Image with the specified name exists. 
void cegui_imgst_isImageDefined(struct hg3dclass_struct * thisclass_c, char * name_c, int * result_c);

// remove the definition for the Image with the specified name. If no such Image exists, nothing happens. 
void cegui_imgst_undefineImage(struct hg3dclass_struct * thisclass_c, char * name_c);

// Removes the definitions for all Image objects currently defined in the Imageset
void cegui_imgst_undefineAllImages(struct hg3dclass_struct * thisclass_c);

// return the width of the named image. 
void cegui_imgst_getImageWidth(struct hg3dclass_struct * thisclass_c, char * name_c, float * result_c);

// return the height of the named image. 
void cegui_imgst_getImageHeight(struct hg3dclass_struct * thisclass_c, char * name_c, float * result_c);

// return the x rendering offset for the named image. 
void cegui_imgst_getImageOffsetX(struct hg3dclass_struct * thisclass_c, char * name_c, float * result_c);

// return the y rendering offset for the named image. 
void cegui_imgst_getImageOffsetY(struct hg3dclass_struct * thisclass_c, char * name_c, float * result_c);

// Return whether this Imageset
void cegui_imgst_isAutoScaled(struct hg3dclass_struct * thisclass_c, int * result_c);

// Enable or disable auto-scaling for this Imageset
void cegui_imgst_setAutoScalingEnabled(struct hg3dclass_struct * thisclass_c, int setting_c);

// Sets the default resource group to be used when loading imageset data. 
void cegui_imgst_setDefaultResourceGroup(char * resourceGroup_c);

// Returns the default resource group currently set for Imagesets. 
void cegui_imgst_getDefaultResourceGroup(char * result_c);

#endif 
