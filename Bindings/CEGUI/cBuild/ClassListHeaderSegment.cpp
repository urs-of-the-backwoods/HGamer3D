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

// ClassListHeaderSegment.cpp

// 

#include <wchar.h>
#include <string>
#include <iostream>

#include <iostream>
	#include <typeinfo>
	#include <stdio.h>
	#include <cstring>
	#include <exception>
	#include "CEGUIDllDefines.h"
	#include "ClassPtr.h"
	#include "EnumSortDirection.h"
#include "./CEGUI.h"
#include "./CEGUIString.h"
#include "RendererModules/Ogre/CEGUIOgreRenderer.h"
#include "./WindowManagerHG3D.h"
#include "./SystemHG3D.h"
#include "HG3DCommandHandler.h"
#include "HG3DEventStaticFunctions.h"
#include "HG3DListboxStaticFunctions.h"
#include "HG3DEventController.h"
#include "HG3DEventModule.h"
#include "HG3DWindowStaticFunctions.h"

using namespace CEGUI;



// Return whether this segment can be sized. 
extern "C" CEGUI_LIB_EXPORT void cegui_lsthdrsgm_isSizingEnabled(struct hg3dclass_struct * thisclass_c, int * result_c)
{
  CEGUI::ListHeaderSegment * thisclass_cpp = static_cast<CEGUI::ListHeaderSegment*> (getHG3DClassPtr(*thisclass_c, "CEGUI::ListHeaderSegment"));
  bool result_cpp;
  result_cpp = (thisclass_cpp->isSizingEnabled());
  *result_c = (int)result_cpp;
};

// Return the current sort direction set for this segment. 
extern "C" CEGUI_LIB_EXPORT void cegui_lsthdrsgm_getSortDirection(struct hg3dclass_struct * thisclass_c, enum EnumSortDirection * result_c)
{
  CEGUI::ListHeaderSegment * thisclass_cpp = static_cast<CEGUI::ListHeaderSegment*> (getHG3DClassPtr(*thisclass_c, "CEGUI::ListHeaderSegment"));
  enum CEGUI::ListHeaderSegment::SortDirection result_cpp;
  result_cpp = (thisclass_cpp->getSortDirection());
  *result_c = (enum EnumSortDirection) result_cpp;
};

// Return whether drag moving is enabled for this segment. 
extern "C" CEGUI_LIB_EXPORT void cegui_lsthdrsgm_isDragMovingEnabled(struct hg3dclass_struct * thisclass_c, int * result_c)
{
  CEGUI::ListHeaderSegment * thisclass_cpp = static_cast<CEGUI::ListHeaderSegment*> (getHG3DClassPtr(*thisclass_c, "CEGUI::ListHeaderSegment"));
  bool result_cpp;
  result_cpp = (thisclass_cpp->isDragMovingEnabled());
  *result_c = (int)result_cpp;
};

// Return whether the segment is clickable. 
extern "C" CEGUI_LIB_EXPORT void cegui_lsthdrsgm_isClickable(struct hg3dclass_struct * thisclass_c, int * result_c)
{
  CEGUI::ListHeaderSegment * thisclass_cpp = static_cast<CEGUI::ListHeaderSegment*> (getHG3DClassPtr(*thisclass_c, "CEGUI::ListHeaderSegment"));
  bool result_cpp;
  result_cpp = (thisclass_cpp->isClickable());
  *result_c = (int)result_cpp;
};

// Return whether the segment is currently in its hovering state. 
extern "C" CEGUI_LIB_EXPORT void cegui_lsthdrsgm_isSegmentHovering(struct hg3dclass_struct * thisclass_c, int * result_c)
{
  CEGUI::ListHeaderSegment * thisclass_cpp = static_cast<CEGUI::ListHeaderSegment*> (getHG3DClassPtr(*thisclass_c, "CEGUI::ListHeaderSegment"));
  bool result_cpp;
  result_cpp = (thisclass_cpp->isSegmentHovering());
  *result_c = (int)result_cpp;
};

// Return whether the segment is currently in its pushed state. 
extern "C" CEGUI_LIB_EXPORT void cegui_lsthdrsgm_isSegmentPushed(struct hg3dclass_struct * thisclass_c, int * result_c)
{
  CEGUI::ListHeaderSegment * thisclass_cpp = static_cast<CEGUI::ListHeaderSegment*> (getHG3DClassPtr(*thisclass_c, "CEGUI::ListHeaderSegment"));
  bool result_cpp;
  result_cpp = (thisclass_cpp->isSegmentPushed());
  *result_c = (int)result_cpp;
};

// Return whether the splitter is currently in its hovering state. 
extern "C" CEGUI_LIB_EXPORT void cegui_lsthdrsgm_isSplitterHovering(struct hg3dclass_struct * thisclass_c, int * result_c)
{
  CEGUI::ListHeaderSegment * thisclass_cpp = static_cast<CEGUI::ListHeaderSegment*> (getHG3DClassPtr(*thisclass_c, "CEGUI::ListHeaderSegment"));
  bool result_cpp;
  result_cpp = (thisclass_cpp->isSplitterHovering());
  *result_c = (int)result_cpp;
};

// Return whether the segment is currently being drag-moved. 
extern "C" CEGUI_LIB_EXPORT void cegui_lsthdrsgm_isBeingDragMoved(struct hg3dclass_struct * thisclass_c, int * result_c)
{
  CEGUI::ListHeaderSegment * thisclass_cpp = static_cast<CEGUI::ListHeaderSegment*> (getHG3DClassPtr(*thisclass_c, "CEGUI::ListHeaderSegment"));
  bool result_cpp;
  result_cpp = (thisclass_cpp->isBeingDragMoved());
  *result_c = (int)result_cpp;
};

// Return whether the segment is currently being drag-moved. 
extern "C" CEGUI_LIB_EXPORT void cegui_lsthdrsgm_isBeingDragSized(struct hg3dclass_struct * thisclass_c, int * result_c)
{
  CEGUI::ListHeaderSegment * thisclass_cpp = static_cast<CEGUI::ListHeaderSegment*> (getHG3DClassPtr(*thisclass_c, "CEGUI::ListHeaderSegment"));
  bool result_cpp;
  result_cpp = (thisclass_cpp->isBeingDragSized());
  *result_c = (int)result_cpp;
};

// Set whether this segment can be sized. 
extern "C" CEGUI_LIB_EXPORT void cegui_lsthdrsgm_setSizingEnabled(struct hg3dclass_struct * thisclass_c, int setting_c)
{
  CEGUI::ListHeaderSegment * thisclass_cpp = static_cast<CEGUI::ListHeaderSegment*> (getHG3DClassPtr(*thisclass_c, "CEGUI::ListHeaderSegment"));
  bool setting_cpp = (bool)setting_c;
  (thisclass_cpp->setSizingEnabled(setting_cpp));
};

// Set the current sort direction set for this segment. 
extern "C" CEGUI_LIB_EXPORT void cegui_lsthdrsgm_setSortDirection(struct hg3dclass_struct * thisclass_c, enum EnumSortDirection sort_dir_c)
{
  CEGUI::ListHeaderSegment * thisclass_cpp = static_cast<CEGUI::ListHeaderSegment*> (getHG3DClassPtr(*thisclass_c, "CEGUI::ListHeaderSegment"));
  enum CEGUI::ListHeaderSegment::SortDirection sort_dir_cpp = (enum CEGUI::ListHeaderSegment::SortDirection)sort_dir_c;
  (thisclass_cpp->setSortDirection(sort_dir_cpp));
};

// Set whether drag moving is allowed for this segment. 
extern "C" CEGUI_LIB_EXPORT void cegui_lsthdrsgm_setDragMovingEnabled(struct hg3dclass_struct * thisclass_c, int setting_c)
{
  CEGUI::ListHeaderSegment * thisclass_cpp = static_cast<CEGUI::ListHeaderSegment*> (getHG3DClassPtr(*thisclass_c, "CEGUI::ListHeaderSegment"));
  bool setting_cpp = (bool)setting_c;
  (thisclass_cpp->setDragMovingEnabled(setting_cpp));
};

// Set whether the segment is clickable. 
extern "C" CEGUI_LIB_EXPORT void cegui_lsthdrsgm_setClickable(struct hg3dclass_struct * thisclass_c, int setting_c)
{
  CEGUI::ListHeaderSegment * thisclass_cpp = static_cast<CEGUI::ListHeaderSegment*> (getHG3DClassPtr(*thisclass_c, "CEGUI::ListHeaderSegment"));
  bool setting_cpp = (bool)setting_c;
  (thisclass_cpp->setClickable(setting_cpp));
};

// 
extern "C" CEGUI_LIB_EXPORT void cegui_lsthdrsgm_setSizingCursorImage2(struct hg3dclass_struct * thisclass_c, char * imageset_c, char * image_c)
{
  CEGUI::ListHeaderSegment * thisclass_cpp = static_cast<CEGUI::ListHeaderSegment*> (getHG3DClassPtr(*thisclass_c, "CEGUI::ListHeaderSegment"));
  CEGUI::String imageset_cpp = CEGUI::String((const char*) imageset_c);
  CEGUI::String image_cpp = CEGUI::String((const char*) image_c);
  (thisclass_cpp->setSizingCursorImage(imageset_cpp, image_cpp));
};

// 
extern "C" CEGUI_LIB_EXPORT void cegui_lsthdrsgm_setMovingCursorImage2(struct hg3dclass_struct * thisclass_c, char * imageset_c, char * image_c)
{
  CEGUI::ListHeaderSegment * thisclass_cpp = static_cast<CEGUI::ListHeaderSegment*> (getHG3DClassPtr(*thisclass_c, "CEGUI::ListHeaderSegment"));
  CEGUI::String imageset_cpp = CEGUI::String((const char*) imageset_c);
  CEGUI::String image_cpp = CEGUI::String((const char*) image_c);
  (thisclass_cpp->setMovingCursorImage(imageset_cpp, image_cpp));
};

// Constructor for list header segment base class. 
extern "C" CEGUI_LIB_EXPORT void cegui_lsthdrsgm_construct(char * type_c, char * name_c, struct hg3dclass_struct * result_c)
{
  CEGUI::String type_cpp = CEGUI::String((const char*) type_c);
  CEGUI::String name_cpp = CEGUI::String((const char*) name_c);
  CEGUI::ListHeaderSegment * result_cpp;
  result_cpp = (new CEGUI::ListHeaderSegment(type_cpp, name_cpp));
  *result_c = getHG3DClass_ListHeaderSegment((void *) result_cpp);
;
};

// Destructor for list header segment base class. 
extern "C" CEGUI_LIB_EXPORT void cegui_lsthdrsgm_destruct(struct hg3dclass_struct * thisclass_c)
{
  CEGUI::ListHeaderSegment * thisclass_cpp = static_cast<CEGUI::ListHeaderSegment*> (getHG3DClassPtr(*thisclass_c, "CEGUI::ListHeaderSegment"));
  (delete thisclass_cpp);
};

