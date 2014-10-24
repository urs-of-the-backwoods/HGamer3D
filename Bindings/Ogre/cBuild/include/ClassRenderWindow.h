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

// ClassRenderWindow.h

// 

#include "wchar.h"
#ifndef _DEFINED_HG3D_ClassRenderWindow
#define _DEFINED_HG3D_ClassRenderWindow

#include "ClassPtr.h"


// 
void ogre_rw_setFullscreen(struct hg3dclass_struct * thisclass_c, long fullScreen_c, unsigned long width_c, unsigned long height_c);

// 
void ogre_rw_destroy(struct hg3dclass_struct * thisclass_c);

// 
void ogre_rw_resize(struct hg3dclass_struct * thisclass_c, unsigned long width_c, unsigned long height_c);

// 
void ogre_rw_windowMovedOrResized(struct hg3dclass_struct * thisclass_c);

// 
void ogre_rw_reposition(struct hg3dclass_struct * thisclass_c, long left_c, long top_c);

// 
void ogre_rw_isVisible(struct hg3dclass_struct * thisclass_c, long * result_c);

// 
void ogre_rw_setVisible(struct hg3dclass_struct * thisclass_c, long visible_c);

// 
void ogre_rw_isHidden(struct hg3dclass_struct * thisclass_c, long * result_c);

// 
void ogre_rw_setHidden(struct hg3dclass_struct * thisclass_c, long hidden_c);

// 
void ogre_rw_setVSyncEnabled(struct hg3dclass_struct * thisclass_c, long vsync_c);

// 
void ogre_rw_isVSyncEnabled(struct hg3dclass_struct * thisclass_c, long * result_c);

// 
void ogre_rw_setVSyncInterval(struct hg3dclass_struct * thisclass_c, unsigned long interval_c);

// 
void ogre_rw_getVSyncInterval(struct hg3dclass_struct * thisclass_c, unsigned long * result_c);

// 
void ogre_rw_isActive(struct hg3dclass_struct * thisclass_c, long * result_c);

// 
void ogre_rw_isClosed(struct hg3dclass_struct * thisclass_c, long * result_c);

// 
void ogre_rw_isPrimary(struct hg3dclass_struct * thisclass_c, long * result_c);

// 
void ogre_rw_isFullScreen(struct hg3dclass_struct * thisclass_c, long * result_c);

// 
void ogre_rw_getMetrics(struct hg3dclass_struct * thisclass_c, unsigned long * width_c, unsigned long * height_c, unsigned long * colourDepth_c, long * left_c, long * top_c);

// 
void ogre_rw_isDeactivatedOnFocusChange(struct hg3dclass_struct * thisclass_c, long * result_c);

// 
void ogre_rw_setDeactivateOnFocusChange(struct hg3dclass_struct * thisclass_c, long deactivate_c);

#endif 
