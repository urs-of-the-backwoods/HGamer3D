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

// ClassSystem.cpp

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
	#include "EnumMouseButton.h"
#include "./CEGUI.h"
#include "./CEGUIString.h"
#include "RendererModules/Ogre/CEGUIOgreRenderer.h"
#include "./WindowManagerHG3D.h"
#include "./SystemHG3D.h"
#include "HG3DEventStaticFunctions.h"
#include "HG3DListboxStaticFunctions.h"
#include "HG3DEventController.h"
#include "HG3DCommandHandler.h"
#include "HG3DEventModule.h"
#include "HG3DWindowStaticFunctions.h"

using namespace CEGUI;



// Create the System
extern "C" CEGUI_LIB_EXPORT void cegui_sstm_create(struct hg3dclass_struct * renderer_c, struct hg3dclass_struct * resourceProvider_c, struct hg3dclass_struct * xmlParser_c, struct hg3dclass_struct * imageCodec_c, struct hg3dclass_struct * scriptModule_c, char * configFile_c, char * logFile_c, struct hg3dclass_struct * result_c)
{
  CEGUI::Renderer * renderer_cpp = static_cast<CEGUI::Renderer*> (getHG3DClassPtr(*renderer_c, "CEGUI::Renderer"));
  CEGUI::ResourceProvider * resourceProvider_cpp = static_cast<CEGUI::ResourceProvider*> (getHG3DClassPtr(*resourceProvider_c, "CEGUI::ResourceProvider"));
  CEGUI::XMLParser * xmlParser_cpp = static_cast<CEGUI::XMLParser*> (getHG3DClassPtr(*xmlParser_c, "CEGUI::XMLParser"));
  CEGUI::ImageCodec * imageCodec_cpp = static_cast<CEGUI::ImageCodec*> (getHG3DClassPtr(*imageCodec_c, "CEGUI::ImageCodec"));
  CEGUI::ScriptModule * scriptModule_cpp = static_cast<CEGUI::ScriptModule*> (getHG3DClassPtr(*scriptModule_c, "CEGUI::ScriptModule"));
  CEGUI::String configFile_cpp = CEGUI::String((const char*) configFile_c);
  CEGUI::String logFile_cpp = CEGUI::String((const char*) logFile_c);
  CEGUI::System * result_cpp;
  result_cpp = &(CEGUI::System::create(*renderer_cpp, resourceProvider_cpp, xmlParser_cpp, imageCodec_cpp, scriptModule_cpp, configFile_cpp, logFile_cpp));
  *result_c = getHG3DClass_System((void *) result_cpp);
;
};

// Destroy the System
extern "C" CEGUI_LIB_EXPORT void cegui_sstm_destroy()
{
  (CEGUI::System::destroy());
};

// Return singleton System
extern "C" CEGUI_LIB_EXPORT void cegui_sstm_getSingleton(struct hg3dclass_struct * result_c)
{
  CEGUI::System * result_cpp;
  result_cpp = &(CEGUI::System::getSingleton());
  *result_c = getHG3DClass_System((void *) result_cpp);
;
};

// Return pointer to singleton System
extern "C" CEGUI_LIB_EXPORT void cegui_sstm_getSingletonPtr(struct hg3dclass_struct * result_c)
{
  CEGUI::System * result_cpp;
  result_cpp = (CEGUI::System::getSingletonPtr());
  *result_c = getHG3DClass_System((void *) result_cpp);
;
};

// Static member to set the name of the default XML parser module that should be used. 
extern "C" CEGUI_LIB_EXPORT void cegui_sstm_setDefaultXMLParserName(char * parserName_c)
{
  CEGUI::String parserName_cpp = CEGUI::String((const char*) parserName_c);
  (CEGUI::System::setDefaultXMLParserName(parserName_cpp));
};

// Return the name of the currently set default xml parser module. 
extern "C" CEGUI_LIB_EXPORT void cegui_sstm_getDefaultXMLParserName(char * result_c)
{
  CEGUI::String result_cpp;
  result_cpp = (CEGUI::System::getDefaultXMLParserName());
  if (strlen( (char *) result_cpp.c_str()) < (1024 * 64 - 1))  { 
    strcpy(result_c, (char *) result_cpp.c_str()); 
  } else {
    strcpy(result_c, "error: outstring larger then 64k");
  };
};

// Set the name of the default image codec to be used. 
extern "C" CEGUI_LIB_EXPORT void cegui_sstm_setDefaultImageCodecName(char * codecName_c)
{
  CEGUI::String codecName_cpp = CEGUI::String((const char*) codecName_c);
  (CEGUI::System::setDefaultImageCodecName(codecName_cpp));
};

// Get the name of the default image codec. 
extern "C" CEGUI_LIB_EXPORT void cegui_sstm_getDefaultImageCodecName(char * result_c)
{
  CEGUI::String result_cpp;
  result_cpp = (CEGUI::System::getDefaultImageCodecName());
  if (strlen( (char *) result_cpp.c_str()) < (1024 * 64 - 1))  { 
    strcpy(result_c, (char *) result_cpp.c_str()); 
  } else {
    strcpy(result_c, "error: outstring larger then 64k");
  };
};

// Return a pointer to the Renderer
extern "C" CEGUI_LIB_EXPORT void cegui_sstm_getRenderer(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * result_c)
{
  CEGUI::System * thisclass_cpp = static_cast<CEGUI::System*> (getHG3DClassPtr(*thisclass_c, "CEGUI::System"));
  CEGUI::Renderer * result_cpp;
  result_cpp = (thisclass_cpp->getRenderer());
  *result_c = getHG3DClass_Renderer((void *) result_cpp);
;
};

// Set the default font to be used by the system. 
extern "C" CEGUI_LIB_EXPORT void cegui_sstm_setDefaultFont(struct hg3dclass_struct * thisclass_c, char * name_c)
{
  CEGUI::System * thisclass_cpp = static_cast<CEGUI::System*> (getHG3DClassPtr(*thisclass_c, "CEGUI::System"));
  CEGUI::String name_cpp = CEGUI::String((const char*) name_c);
  (thisclass_cpp->setDefaultFont(name_cpp));
};

// Set the default font to be used by the system. 
extern "C" CEGUI_LIB_EXPORT void cegui_sstm_setDefaultFont2(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * font_c)
{
  CEGUI::System * thisclass_cpp = static_cast<CEGUI::System*> (getHG3DClassPtr(*thisclass_c, "CEGUI::System"));
  CEGUI::Font * font_cpp = static_cast<CEGUI::Font*> (getHG3DClassPtr(*font_c, "CEGUI::Font"));
  (thisclass_cpp->setDefaultFont(font_cpp));
};

// Return a pointer to the default Font
extern "C" CEGUI_LIB_EXPORT void cegui_sstm_getDefaultFont(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * result_c)
{
  CEGUI::System * thisclass_cpp = static_cast<CEGUI::System*> (getHG3DClassPtr(*thisclass_c, "CEGUI::System"));
  CEGUI::Font * result_cpp;
  result_cpp = (thisclass_cpp->getDefaultFont());
  *result_c = getHG3DClass_Font((void *) result_cpp);
;
};

// Causes a full re-draw next time renderGUI()
extern "C" CEGUI_LIB_EXPORT void cegui_sstm_signalRedraw(struct hg3dclass_struct * thisclass_c)
{
  CEGUI::System * thisclass_cpp = static_cast<CEGUI::System*> (getHG3DClassPtr(*thisclass_c, "CEGUI::System"));
  (thisclass_cpp->signalRedraw());
};

// Return a boolean value to indicate whether a full re-draw is requested next time renderGUI()
extern "C" CEGUI_LIB_EXPORT void cegui_sstm_isRedrawRequested(struct hg3dclass_struct * thisclass_c, int * result_c)
{
  CEGUI::System * thisclass_cpp = static_cast<CEGUI::System*> (getHG3DClassPtr(*thisclass_c, "CEGUI::System"));
  bool result_cpp;
  result_cpp = (thisclass_cpp->isRedrawRequested());
  *result_c = (int)result_cpp;
};

// Render the GUI. 
extern "C" CEGUI_LIB_EXPORT void cegui_sstm_renderGUI(struct hg3dclass_struct * thisclass_c)
{
  CEGUI::System * thisclass_cpp = static_cast<CEGUI::System*> (getHG3DClassPtr(*thisclass_c, "CEGUI::System"));
  (thisclass_cpp->renderGUI());
};

// Set the active GUI sheet (root) window. 
extern "C" CEGUI_LIB_EXPORT void cegui_sstm_setGUISheet(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * sheet_c, struct hg3dclass_struct * result_c)
{
  CEGUI::System * thisclass_cpp = static_cast<CEGUI::System*> (getHG3DClassPtr(*thisclass_c, "CEGUI::System"));
  CEGUI::Window * sheet_cpp = static_cast<CEGUI::Window*> (getHG3DClassPtr(*sheet_c, "CEGUI::Window"));
  CEGUI::Window * result_cpp;
  result_cpp = (thisclass_cpp->setGUISheet(sheet_cpp));
  *result_c = getHG3DClass_Window((void *) result_cpp);
;
};

// Return a pointer to the active GUI sheet (root) window. 
extern "C" CEGUI_LIB_EXPORT void cegui_sstm_getGUISheet(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * result_c)
{
  CEGUI::System * thisclass_cpp = static_cast<CEGUI::System*> (getHG3DClassPtr(*thisclass_c, "CEGUI::System"));
  CEGUI::Window * result_cpp;
  result_cpp = (thisclass_cpp->getGUISheet());
  *result_c = getHG3DClass_Window((void *) result_cpp);
;
};

// Return the current timeout for generation of single-click events. 
extern "C" CEGUI_LIB_EXPORT void cegui_sstm_getSingleClickTimeout(struct hg3dclass_struct * thisclass_c, double * result_c)
{
  CEGUI::System * thisclass_cpp = static_cast<CEGUI::System*> (getHG3DClassPtr(*thisclass_c, "CEGUI::System"));
  double result_cpp;
  result_cpp = (thisclass_cpp->getSingleClickTimeout());
  *result_c = (double)result_cpp;
};

// Return the current timeout for generation of multi-click events. 
extern "C" CEGUI_LIB_EXPORT void cegui_sstm_getMultiClickTimeout(struct hg3dclass_struct * thisclass_c, double * result_c)
{
  CEGUI::System * thisclass_cpp = static_cast<CEGUI::System*> (getHG3DClassPtr(*thisclass_c, "CEGUI::System"));
  double result_cpp;
  result_cpp = (thisclass_cpp->getMultiClickTimeout());
  *result_c = (double)result_cpp;
};

// Set the timeout used for generation of single-click events. 
extern "C" CEGUI_LIB_EXPORT void cegui_sstm_setSingleClickTimeout(struct hg3dclass_struct * thisclass_c, double timeout_c)
{
  CEGUI::System * thisclass_cpp = static_cast<CEGUI::System*> (getHG3DClassPtr(*thisclass_c, "CEGUI::System"));
  double timeout_cpp = (double)timeout_c;
  (thisclass_cpp->setSingleClickTimeout(timeout_cpp));
};

// Set the timeout to be used for the generation of multi-click events. 
extern "C" CEGUI_LIB_EXPORT void cegui_sstm_setMultiClickTimeout(struct hg3dclass_struct * thisclass_c, double timeout_c)
{
  CEGUI::System * thisclass_cpp = static_cast<CEGUI::System*> (getHG3DClassPtr(*thisclass_c, "CEGUI::System"));
  double timeout_cpp = (double)timeout_c;
  (thisclass_cpp->setMultiClickTimeout(timeout_cpp));
};

// Return whether automatic mouse button click and multi-click (i.e. double-click and treble-click) event generation is enabled. 
extern "C" CEGUI_LIB_EXPORT void cegui_sstm_isMouseClickEventGenerationEnabled(struct hg3dclass_struct * thisclass_c, int * result_c)
{
  CEGUI::System * thisclass_cpp = static_cast<CEGUI::System*> (getHG3DClassPtr(*thisclass_c, "CEGUI::System"));
  bool result_cpp;
  result_cpp = (thisclass_cpp->isMouseClickEventGenerationEnabled());
  *result_c = (int)result_cpp;
};

// Set whether automatic mouse button click and multi-click (i.e. double-click and treble-click) event generation will occur. 
extern "C" CEGUI_LIB_EXPORT void cegui_sstm_setMouseClickEventGenerationEnabled(struct hg3dclass_struct * thisclass_c, const int enable_c)
{
  CEGUI::System * thisclass_cpp = static_cast<CEGUI::System*> (getHG3DClassPtr(*thisclass_c, "CEGUI::System"));
  const bool enable_cpp = (bool)enable_c;
  (thisclass_cpp->setMouseClickEventGenerationEnabled(enable_cpp));
};

// Set the image to be used as the default mouse cursor. 
extern "C" CEGUI_LIB_EXPORT void cegui_sstm_setDefaultMouseCursor3(struct hg3dclass_struct * thisclass_c, char * imageset_c, char * image_name_c)
{
  CEGUI::System * thisclass_cpp = static_cast<CEGUI::System*> (getHG3DClassPtr(*thisclass_c, "CEGUI::System"));
  CEGUI::String imageset_cpp = CEGUI::String((const char*) imageset_c);
  CEGUI::String image_name_cpp = CEGUI::String((const char*) image_name_c);
  (thisclass_cpp->setDefaultMouseCursor(imageset_cpp, image_name_cpp));
};

// Return the Window
extern "C" CEGUI_LIB_EXPORT void cegui_sstm_getWindowContainingMouse(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * result_c)
{
  CEGUI::System * thisclass_cpp = static_cast<CEGUI::System*> (getHG3DClassPtr(*thisclass_c, "CEGUI::System"));
  CEGUI::Window * result_cpp;
  result_cpp = (thisclass_cpp->getWindowContainingMouse());
  *result_c = getHG3DClass_Window((void *) result_cpp);
;
};

// Return a pointer to the ScriptModule
extern "C" CEGUI_LIB_EXPORT void cegui_sstm_getScriptingModule(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * result_c)
{
  CEGUI::System * thisclass_cpp = static_cast<CEGUI::System*> (getHG3DClassPtr(*thisclass_c, "CEGUI::System"));
  CEGUI::ScriptModule * result_cpp;
  result_cpp = (thisclass_cpp->getScriptingModule());
  *result_c = getHG3DClass_ScriptModule((void *) result_cpp);
;
};

// Set the ScriptModule
extern "C" CEGUI_LIB_EXPORT void cegui_sstm_setScriptingModule(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * scriptModule_c)
{
  CEGUI::System * thisclass_cpp = static_cast<CEGUI::System*> (getHG3DClassPtr(*thisclass_c, "CEGUI::System"));
  CEGUI::ScriptModule * scriptModule_cpp = static_cast<CEGUI::ScriptModule*> (getHG3DClassPtr(*scriptModule_c, "CEGUI::ScriptModule"));
  (thisclass_cpp->setScriptingModule(scriptModule_cpp));
};

// Return a pointer to the ResourceProvider
extern "C" CEGUI_LIB_EXPORT void cegui_sstm_getResourceProvider(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * result_c)
{
  CEGUI::System * thisclass_cpp = static_cast<CEGUI::System*> (getHG3DClassPtr(*thisclass_c, "CEGUI::System"));
  CEGUI::ResourceProvider * result_cpp;
  result_cpp = (thisclass_cpp->getResourceProvider());
  *result_c = getHG3DClass_ResourceProvider((void *) result_cpp);
;
};

// Execute a script file if possible. 
extern "C" CEGUI_LIB_EXPORT void cegui_sstm_executeScriptFile(struct hg3dclass_struct * thisclass_c, char * filename_c, char * resourceGroup_c)
{
  CEGUI::System * thisclass_cpp = static_cast<CEGUI::System*> (getHG3DClassPtr(*thisclass_c, "CEGUI::System"));
  CEGUI::String filename_cpp = CEGUI::String((const char*) filename_c);
  CEGUI::String resourceGroup_cpp = CEGUI::String((const char*) resourceGroup_c);
  (thisclass_cpp->executeScriptFile(filename_cpp, resourceGroup_cpp));
};

// Execute a scripted global function if possible. The function should not take any parameters and should return an integer. 
extern "C" CEGUI_LIB_EXPORT void cegui_sstm_executeScriptGlobal(struct hg3dclass_struct * thisclass_c, char * function_name_c, int * result_c)
{
  CEGUI::System * thisclass_cpp = static_cast<CEGUI::System*> (getHG3DClassPtr(*thisclass_c, "CEGUI::System"));
  CEGUI::String function_name_cpp = CEGUI::String((const char*) function_name_c);
  int result_cpp;
  result_cpp = (thisclass_cpp->executeScriptGlobal(function_name_cpp));
  *result_c = (int)result_cpp;
};

// If possible, execute script code contained in the given CEGUI::String object. 
extern "C" CEGUI_LIB_EXPORT void cegui_sstm_executeScriptString(struct hg3dclass_struct * thisclass_c, char * str_c)
{
  CEGUI::System * thisclass_cpp = static_cast<CEGUI::System*> (getHG3DClassPtr(*thisclass_c, "CEGUI::System"));
  CEGUI::String str_cpp = CEGUI::String((const char*) str_c);
  (thisclass_cpp->executeScriptString(str_cpp));
};

// return the current mouse movement scaling factor. 
extern "C" CEGUI_LIB_EXPORT void cegui_sstm_getMouseMoveScaling(struct hg3dclass_struct * thisclass_c, float * result_c)
{
  CEGUI::System * thisclass_cpp = static_cast<CEGUI::System*> (getHG3DClassPtr(*thisclass_c, "CEGUI::System"));
  float result_cpp;
  result_cpp = (thisclass_cpp->getMouseMoveScaling());
  *result_c = (float)result_cpp;
};

// Set the current mouse movement scaling factor. 
extern "C" CEGUI_LIB_EXPORT void cegui_sstm_setMouseMoveScaling(struct hg3dclass_struct * thisclass_c, float scaling_c)
{
  CEGUI::System * thisclass_cpp = static_cast<CEGUI::System*> (getHG3DClassPtr(*thisclass_c, "CEGUI::System"));
  float scaling_cpp = (float)scaling_c;
  (thisclass_cpp->setMouseMoveScaling(scaling_cpp));
};

// Internal method used to inform the SystemSystem
extern "C" CEGUI_LIB_EXPORT void cegui_sstm_notifyWindowDestroyed(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * window_c)
{
  CEGUI::System * thisclass_cpp = static_cast<CEGUI::System*> (getHG3DClassPtr(*thisclass_c, "CEGUI::System"));
  const CEGUI::Window * window_cpp = static_cast<CEGUI::Window*> (getHG3DClassPtr(*window_c, "CEGUI::Window"));
  (thisclass_cpp->notifyWindowDestroyed(window_cpp));
};

// Return the current system keys value. 
extern "C" CEGUI_LIB_EXPORT void cegui_sstm_getSystemKeys(struct hg3dclass_struct * thisclass_c, unsigned int * result_c)
{
  CEGUI::System * thisclass_cpp = static_cast<CEGUI::System*> (getHG3DClassPtr(*thisclass_c, "CEGUI::System"));
  uint result_cpp;
  result_cpp = (thisclass_cpp->getSystemKeys());
  *result_c = (unsigned int)result_cpp;
};

// Set a new XML parser module to be used. 
extern "C" CEGUI_LIB_EXPORT void cegui_sstm_setXMLParser(struct hg3dclass_struct * thisclass_c, char * parserName_c)
{
  CEGUI::System * thisclass_cpp = static_cast<CEGUI::System*> (getHG3DClassPtr(*thisclass_c, "CEGUI::System"));
  CEGUI::String parserName_cpp = CEGUI::String((const char*) parserName_c);
  (thisclass_cpp->setXMLParser(parserName_cpp));
};

// Sets the XMLParser
extern "C" CEGUI_LIB_EXPORT void cegui_sstm_setXMLParser2(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * parser_c)
{
  CEGUI::System * thisclass_cpp = static_cast<CEGUI::System*> (getHG3DClassPtr(*thisclass_c, "CEGUI::System"));
  CEGUI::XMLParser * parser_cpp = static_cast<CEGUI::XMLParser*> (getHG3DClassPtr(*parser_c, "CEGUI::XMLParser"));
  (thisclass_cpp->setXMLParser(parser_cpp));
};

// Return the XMLParser
extern "C" CEGUI_LIB_EXPORT void cegui_sstm_getXMLParser(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * result_c)
{
  CEGUI::System * thisclass_cpp = static_cast<CEGUI::System*> (getHG3DClassPtr(*thisclass_c, "CEGUI::System"));
  CEGUI::XMLParser * result_cpp;
  result_cpp = (thisclass_cpp->getXMLParser());
  *result_c = getHG3DClass_XMLParser((void *) result_cpp);
;
};

// Set the system default TooltipTooltip
extern "C" CEGUI_LIB_EXPORT void cegui_sstm_setDefaultTooltip(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * tooltip_c)
{
  CEGUI::System * thisclass_cpp = static_cast<CEGUI::System*> (getHG3DClassPtr(*thisclass_c, "CEGUI::System"));
  CEGUI::Tooltip * tooltip_cpp = static_cast<CEGUI::Tooltip*> (getHG3DClassPtr(*tooltip_c, "CEGUI::Tooltip"));
  (thisclass_cpp->setDefaultTooltip(tooltip_cpp));
};

// Set the system default TooltipWindow
extern "C" CEGUI_LIB_EXPORT void cegui_sstm_setDefaultTooltip2(struct hg3dclass_struct * thisclass_c, char * tooltipType_c)
{
  CEGUI::System * thisclass_cpp = static_cast<CEGUI::System*> (getHG3DClassPtr(*thisclass_c, "CEGUI::System"));
  CEGUI::String tooltipType_cpp = CEGUI::String((const char*) tooltipType_c);
  (thisclass_cpp->setDefaultTooltip(tooltipType_cpp));
};

// return a poiter to the system default tooltip. May return 0. 
extern "C" CEGUI_LIB_EXPORT void cegui_sstm_getDefaultTooltip(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * result_c)
{
  CEGUI::System * thisclass_cpp = static_cast<CEGUI::System*> (getHG3DClassPtr(*thisclass_c, "CEGUI::System"));
  CEGUI::Tooltip * result_cpp;
  result_cpp = (thisclass_cpp->getDefaultTooltip());
  *result_c = getHG3DClass_Tooltip((void *) result_cpp);
;
};

// Internal method to directly set the current modal target. 
extern "C" CEGUI_LIB_EXPORT void cegui_sstm_setModalTarget(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * target_c)
{
  CEGUI::System * thisclass_cpp = static_cast<CEGUI::System*> (getHG3DClassPtr(*thisclass_c, "CEGUI::System"));
  CEGUI::Window * target_cpp = static_cast<CEGUI::Window*> (getHG3DClassPtr(*target_c, "CEGUI::Window"));
  (thisclass_cpp->setModalTarget(target_cpp));
};

// Return a pointer to the Window
extern "C" CEGUI_LIB_EXPORT void cegui_sstm_getModalTarget(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * result_c)
{
  CEGUI::System * thisclass_cpp = static_cast<CEGUI::System*> (getHG3DClassPtr(*thisclass_c, "CEGUI::System"));
  CEGUI::Window * result_cpp;
  result_cpp = (thisclass_cpp->getModalTarget());
  *result_c = getHG3DClass_Window((void *) result_cpp);
;
};

// Perform updates with regards to the window that contains the mouse cursor, firing any required MouseEnters / MouseLeaves events. 
extern "C" CEGUI_LIB_EXPORT void cegui_sstm_updateWindowContainingMouse(struct hg3dclass_struct * thisclass_c, int * result_c)
{
  CEGUI::System * thisclass_cpp = static_cast<CEGUI::System*> (getHG3DClassPtr(*thisclass_c, "CEGUI::System"));
  bool result_cpp;
  result_cpp = (thisclass_cpp->updateWindowContainingMouse());
  *result_c = (int)result_cpp;
};

// Retrieve the image codec to be used by the system. 
extern "C" CEGUI_LIB_EXPORT void cegui_sstm_getImageCodec(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * result_c)
{
  CEGUI::System * thisclass_cpp = static_cast<CEGUI::System*> (getHG3DClassPtr(*thisclass_c, "CEGUI::System"));
  CEGUI::ImageCodec * result_cpp;
  result_cpp = &(thisclass_cpp->getImageCodec());
  *result_c = getHG3DClass_ImageCodec((void *) result_cpp);
;
};

// Set the image codec to be used by the system. 
extern "C" CEGUI_LIB_EXPORT void cegui_sstm_setImageCodec(struct hg3dclass_struct * thisclass_c, char * codecName_c)
{
  CEGUI::System * thisclass_cpp = static_cast<CEGUI::System*> (getHG3DClassPtr(*thisclass_c, "CEGUI::System"));
  CEGUI::String codecName_cpp = CEGUI::String((const char*) codecName_c);
  (thisclass_cpp->setImageCodec(codecName_cpp));
};

// Set the image codec to use from an existing image codec. 
extern "C" CEGUI_LIB_EXPORT void cegui_sstm_setImageCodec2(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * codec_c)
{
  CEGUI::System * thisclass_cpp = static_cast<CEGUI::System*> (getHG3DClassPtr(*thisclass_c, "CEGUI::System"));
  CEGUI::ImageCodec * codec_cpp = static_cast<CEGUI::ImageCodec*> (getHG3DClassPtr(*codec_c, "CEGUI::ImageCodec"));
  (thisclass_cpp->setImageCodec(*codec_cpp));
};

// Invalidate all imagery and geometry caches for CEGUI
extern "C" CEGUI_LIB_EXPORT void cegui_sstm_invalidateAllCachedRendering(struct hg3dclass_struct * thisclass_c)
{
  CEGUI::System * thisclass_cpp = static_cast<CEGUI::System*> (getHG3DClassPtr(*thisclass_c, "CEGUI::System"));
  (thisclass_cpp->invalidateAllCachedRendering());
};

// Method that injects a mouse movement event into the system. 
extern "C" CEGUI_LIB_EXPORT void cegui_sstm_injectMouseMove(struct hg3dclass_struct * thisclass_c, float delta_x_c, float delta_y_c, int * result_c)
{
  CEGUI::System * thisclass_cpp = static_cast<CEGUI::System*> (getHG3DClassPtr(*thisclass_c, "CEGUI::System"));
  float delta_x_cpp = (float)delta_x_c;
  float delta_y_cpp = (float)delta_y_c;
  bool result_cpp;
  result_cpp = (thisclass_cpp->injectMouseMove(delta_x_cpp, delta_y_cpp));
  *result_c = (int)result_cpp;
};

// Method that injects that the mouse has left the application window. 
extern "C" CEGUI_LIB_EXPORT void cegui_sstm_injectMouseLeaves(struct hg3dclass_struct * thisclass_c, int * result_c)
{
  CEGUI::System * thisclass_cpp = static_cast<CEGUI::System*> (getHG3DClassPtr(*thisclass_c, "CEGUI::System"));
  bool result_cpp;
  result_cpp = (thisclass_cpp->injectMouseLeaves());
  *result_c = (int)result_cpp;
};

// Method that injects a mouse button down event into the system. 
extern "C" CEGUI_LIB_EXPORT void cegui_sstm_injectMouseButtonDown(struct hg3dclass_struct * thisclass_c, enum EnumMouseButton button_c, int * result_c)
{
  CEGUI::System * thisclass_cpp = static_cast<CEGUI::System*> (getHG3DClassPtr(*thisclass_c, "CEGUI::System"));
  enum CEGUI::MouseButton button_cpp = (enum CEGUI::MouseButton)button_c;
  bool result_cpp;
  result_cpp = (thisclass_cpp->injectMouseButtonDown(button_cpp));
  *result_c = (int)result_cpp;
};

// Method that injects a mouse button up event into the system. 
extern "C" CEGUI_LIB_EXPORT void cegui_sstm_injectMouseButtonUp(struct hg3dclass_struct * thisclass_c, enum EnumMouseButton button_c, int * result_c)
{
  CEGUI::System * thisclass_cpp = static_cast<CEGUI::System*> (getHG3DClassPtr(*thisclass_c, "CEGUI::System"));
  enum CEGUI::MouseButton button_cpp = (enum CEGUI::MouseButton)button_c;
  bool result_cpp;
  result_cpp = (thisclass_cpp->injectMouseButtonUp(button_cpp));
  *result_c = (int)result_cpp;
};

// Method that injects a key down event into the system. 
extern "C" CEGUI_LIB_EXPORT void cegui_sstm_injectKeyDown(struct hg3dclass_struct * thisclass_c, unsigned int key_code_c, int * result_c)
{
  CEGUI::System * thisclass_cpp = static_cast<CEGUI::System*> (getHG3DClassPtr(*thisclass_c, "CEGUI::System"));
  uint key_code_cpp = (uint)key_code_c;
  bool result_cpp;
  result_cpp = (thisclass_cpp->injectKeyDown(key_code_cpp));
  *result_c = (int)result_cpp;
};

// Method that injects a key up event into the system. 
extern "C" CEGUI_LIB_EXPORT void cegui_sstm_injectKeyUp(struct hg3dclass_struct * thisclass_c, unsigned int key_code_c, int * result_c)
{
  CEGUI::System * thisclass_cpp = static_cast<CEGUI::System*> (getHG3DClassPtr(*thisclass_c, "CEGUI::System"));
  uint key_code_cpp = (uint)key_code_c;
  bool result_cpp;
  result_cpp = (thisclass_cpp->injectKeyUp(key_code_cpp));
  *result_c = (int)result_cpp;
};

// Method that injects a typed character event into the system. 
extern "C" CEGUI_LIB_EXPORT void cegui_sstm_injectChar(struct hg3dclass_struct * thisclass_c, int code_point_c, int * result_c)
{
  CEGUI::System * thisclass_cpp = static_cast<CEGUI::System*> (getHG3DClassPtr(*thisclass_c, "CEGUI::System"));
  utf32 code_point_cpp = (utf32)code_point_c;
  bool result_cpp;
  result_cpp = (thisclass_cpp->injectChar(code_point_cpp));
  *result_c = (int)result_cpp;
};

// Method that injects a mouse-wheel / scroll-wheel event into the system. 
extern "C" CEGUI_LIB_EXPORT void cegui_sstm_injectMouseWheelChange(struct hg3dclass_struct * thisclass_c, float delta_c, int * result_c)
{
  CEGUI::System * thisclass_cpp = static_cast<CEGUI::System*> (getHG3DClassPtr(*thisclass_c, "CEGUI::System"));
  float delta_cpp = (float)delta_c;
  bool result_cpp;
  result_cpp = (thisclass_cpp->injectMouseWheelChange(delta_cpp));
  *result_c = (int)result_cpp;
};

// Method that injects a new position for the mouse cursor. 
extern "C" CEGUI_LIB_EXPORT void cegui_sstm_injectMousePosition(struct hg3dclass_struct * thisclass_c, float x_pos_c, float y_pos_c, int * result_c)
{
  CEGUI::System * thisclass_cpp = static_cast<CEGUI::System*> (getHG3DClassPtr(*thisclass_c, "CEGUI::System"));
  float x_pos_cpp = (float)x_pos_c;
  float y_pos_cpp = (float)y_pos_c;
  bool result_cpp;
  result_cpp = (thisclass_cpp->injectMousePosition(x_pos_cpp, y_pos_cpp));
  *result_c = (int)result_cpp;
};

// Method to inject time pulses into the system. 
extern "C" CEGUI_LIB_EXPORT void cegui_sstm_injectTimePulse(struct hg3dclass_struct * thisclass_c, float timeElapsed_c, int * result_c)
{
  CEGUI::System * thisclass_cpp = static_cast<CEGUI::System*> (getHG3DClassPtr(*thisclass_c, "CEGUI::System"));
  float timeElapsed_cpp = (float)timeElapsed_c;
  bool result_cpp;
  result_cpp = (thisclass_cpp->injectTimePulse(timeElapsed_cpp));
  *result_c = (int)result_cpp;
};

// Function to directly inject a mouse button click event. 
extern "C" CEGUI_LIB_EXPORT void cegui_sstm_injectMouseButtonClick(struct hg3dclass_struct * thisclass_c, const enum EnumMouseButton button_c, int * result_c)
{
  CEGUI::System * thisclass_cpp = static_cast<CEGUI::System*> (getHG3DClassPtr(*thisclass_c, "CEGUI::System"));
  enum CEGUI::MouseButton button_cpp = (enum CEGUI::MouseButton)button_c;
  bool result_cpp;
  result_cpp = (thisclass_cpp->injectMouseButtonClick(button_cpp));
  *result_c = (int)result_cpp;
};

// Function to directly inject a mouse button double-click event. 
extern "C" CEGUI_LIB_EXPORT void cegui_sstm_injectMouseButtonDoubleClick(struct hg3dclass_struct * thisclass_c, const enum EnumMouseButton button_c, int * result_c)
{
  CEGUI::System * thisclass_cpp = static_cast<CEGUI::System*> (getHG3DClassPtr(*thisclass_c, "CEGUI::System"));
  enum CEGUI::MouseButton button_cpp = (enum CEGUI::MouseButton)button_c;
  bool result_cpp;
  result_cpp = (thisclass_cpp->injectMouseButtonDoubleClick(button_cpp));
  *result_c = (int)result_cpp;
};

// Function to directly inject a mouse button triple-click event. 
extern "C" CEGUI_LIB_EXPORT void cegui_sstm_injectMouseButtonTripleClick(struct hg3dclass_struct * thisclass_c, const enum EnumMouseButton button_c, int * result_c)
{
  CEGUI::System * thisclass_cpp = static_cast<CEGUI::System*> (getHG3DClassPtr(*thisclass_c, "CEGUI::System"));
  enum CEGUI::MouseButton button_cpp = (enum CEGUI::MouseButton)button_c;
  bool result_cpp;
  result_cpp = (thisclass_cpp->injectMouseButtonTripleClick(button_cpp));
  *result_c = (int)result_cpp;
};

