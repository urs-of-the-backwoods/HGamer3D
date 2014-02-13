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

// ClassSystem.h

// 

#include "wchar.h"
#ifndef _DEFINED_HG3D_ClassSystem
#define _DEFINED_HG3D_ClassSystem

#include "ClassPtr.h"
#include "ClassRenderer.h"
#include "ClassResourceProvider.h"
#include "ClassXMLParser.h"
#include "ClassImageCodec.h"
#include "ClassScriptModule.h"
#include "ClassFont.h"
#include "ClassWindow.h"
#include "ClassTooltip.h"
#include "EnumMouseButton.h"


// Create the System
void cegui_sstm_create(struct hg3dclass_struct * renderer_c, struct hg3dclass_struct * resourceProvider_c, struct hg3dclass_struct * xmlParser_c, struct hg3dclass_struct * imageCodec_c, struct hg3dclass_struct * scriptModule_c, char * configFile_c, char * logFile_c, struct hg3dclass_struct * result_c);

// Destroy the System
void cegui_sstm_destroy();

// Return singleton System
void cegui_sstm_getSingleton(struct hg3dclass_struct * result_c);

// Return pointer to singleton System
void cegui_sstm_getSingletonPtr(struct hg3dclass_struct * result_c);

// Static member to set the name of the default XML parser module that should be used. 
void cegui_sstm_setDefaultXMLParserName(char * parserName_c);

// Return the name of the currently set default xml parser module. 
void cegui_sstm_getDefaultXMLParserName(char * result_c);

// Set the name of the default image codec to be used. 
void cegui_sstm_setDefaultImageCodecName(char * codecName_c);

// Get the name of the default image codec. 
void cegui_sstm_getDefaultImageCodecName(char * result_c);

// Return a pointer to the Renderer
void cegui_sstm_getRenderer(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * result_c);

// Set the default font to be used by the system. 
void cegui_sstm_setDefaultFont(struct hg3dclass_struct * thisclass_c, char * name_c);

// Set the default font to be used by the system. 
void cegui_sstm_setDefaultFont2(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * font_c);

// Return a pointer to the default Font
void cegui_sstm_getDefaultFont(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * result_c);

// Causes a full re-draw next time renderGUI()
void cegui_sstm_signalRedraw(struct hg3dclass_struct * thisclass_c);

// Return a boolean value to indicate whether a full re-draw is requested next time renderGUI()
void cegui_sstm_isRedrawRequested(struct hg3dclass_struct * thisclass_c, int * result_c);

// Render the GUI. 
void cegui_sstm_renderGUI(struct hg3dclass_struct * thisclass_c);

// Set the active GUI sheet (root) window. 
void cegui_sstm_setGUISheet(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * sheet_c, struct hg3dclass_struct * result_c);

// Return a pointer to the active GUI sheet (root) window. 
void cegui_sstm_getGUISheet(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * result_c);

// Return the current timeout for generation of single-click events. 
void cegui_sstm_getSingleClickTimeout(struct hg3dclass_struct * thisclass_c, double * result_c);

// Return the current timeout for generation of multi-click events. 
void cegui_sstm_getMultiClickTimeout(struct hg3dclass_struct * thisclass_c, double * result_c);

// Set the timeout used for generation of single-click events. 
void cegui_sstm_setSingleClickTimeout(struct hg3dclass_struct * thisclass_c, double timeout_c);

// Set the timeout to be used for the generation of multi-click events. 
void cegui_sstm_setMultiClickTimeout(struct hg3dclass_struct * thisclass_c, double timeout_c);

// Return whether automatic mouse button click and multi-click (i.e. double-click and treble-click) event generation is enabled. 
void cegui_sstm_isMouseClickEventGenerationEnabled(struct hg3dclass_struct * thisclass_c, int * result_c);

// Set whether automatic mouse button click and multi-click (i.e. double-click and treble-click) event generation will occur. 
void cegui_sstm_setMouseClickEventGenerationEnabled(struct hg3dclass_struct * thisclass_c, const int enable_c);

// Set the image to be used as the default mouse cursor. 
void cegui_sstm_setDefaultMouseCursor3(struct hg3dclass_struct * thisclass_c, char * imageset_c, char * image_name_c);

// Return the Window
void cegui_sstm_getWindowContainingMouse(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * result_c);

// Return a pointer to the ScriptModule
void cegui_sstm_getScriptingModule(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * result_c);

// Set the ScriptModule
void cegui_sstm_setScriptingModule(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * scriptModule_c);

// Return a pointer to the ResourceProvider
void cegui_sstm_getResourceProvider(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * result_c);

// Execute a script file if possible. 
void cegui_sstm_executeScriptFile(struct hg3dclass_struct * thisclass_c, char * filename_c, char * resourceGroup_c);

// Execute a scripted global function if possible. The function should not take any parameters and should return an integer. 
void cegui_sstm_executeScriptGlobal(struct hg3dclass_struct * thisclass_c, char * function_name_c, int * result_c);

// If possible, execute script code contained in the given CEGUI::String object. 
void cegui_sstm_executeScriptString(struct hg3dclass_struct * thisclass_c, char * str_c);

// return the current mouse movement scaling factor. 
void cegui_sstm_getMouseMoveScaling(struct hg3dclass_struct * thisclass_c, float * result_c);

// Set the current mouse movement scaling factor. 
void cegui_sstm_setMouseMoveScaling(struct hg3dclass_struct * thisclass_c, float scaling_c);

// Internal method used to inform the SystemSystem
void cegui_sstm_notifyWindowDestroyed(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * window_c);

// Return the current system keys value. 
void cegui_sstm_getSystemKeys(struct hg3dclass_struct * thisclass_c, unsigned int * result_c);

// Set a new XML parser module to be used. 
void cegui_sstm_setXMLParser(struct hg3dclass_struct * thisclass_c, char * parserName_c);

// Sets the XMLParser
void cegui_sstm_setXMLParser2(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * parser_c);

// Return the XMLParser
void cegui_sstm_getXMLParser(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * result_c);

// Set the system default TooltipTooltip
void cegui_sstm_setDefaultTooltip(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * tooltip_c);

// Set the system default TooltipWindow
void cegui_sstm_setDefaultTooltip2(struct hg3dclass_struct * thisclass_c, char * tooltipType_c);

// return a poiter to the system default tooltip. May return 0. 
void cegui_sstm_getDefaultTooltip(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * result_c);

// Internal method to directly set the current modal target. 
void cegui_sstm_setModalTarget(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * target_c);

// Return a pointer to the Window
void cegui_sstm_getModalTarget(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * result_c);

// Perform updates with regards to the window that contains the mouse cursor, firing any required MouseEnters / MouseLeaves events. 
void cegui_sstm_updateWindowContainingMouse(struct hg3dclass_struct * thisclass_c, int * result_c);

// Retrieve the image codec to be used by the system. 
void cegui_sstm_getImageCodec(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * result_c);

// Set the image codec to be used by the system. 
void cegui_sstm_setImageCodec(struct hg3dclass_struct * thisclass_c, char * codecName_c);

// Set the image codec to use from an existing image codec. 
void cegui_sstm_setImageCodec2(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * codec_c);

// Invalidate all imagery and geometry caches for CEGUI
void cegui_sstm_invalidateAllCachedRendering(struct hg3dclass_struct * thisclass_c);

// Method that injects a mouse movement event into the system. 
void cegui_sstm_injectMouseMove(struct hg3dclass_struct * thisclass_c, float delta_x_c, float delta_y_c, int * result_c);

// Method that injects that the mouse has left the application window. 
void cegui_sstm_injectMouseLeaves(struct hg3dclass_struct * thisclass_c, int * result_c);

// Method that injects a mouse button down event into the system. 
void cegui_sstm_injectMouseButtonDown(struct hg3dclass_struct * thisclass_c, enum EnumMouseButton button_c, int * result_c);

// Method that injects a mouse button up event into the system. 
void cegui_sstm_injectMouseButtonUp(struct hg3dclass_struct * thisclass_c, enum EnumMouseButton button_c, int * result_c);

// Method that injects a key down event into the system. 
void cegui_sstm_injectKeyDown(struct hg3dclass_struct * thisclass_c, unsigned int key_code_c, int * result_c);

// Method that injects a key up event into the system. 
void cegui_sstm_injectKeyUp(struct hg3dclass_struct * thisclass_c, unsigned int key_code_c, int * result_c);

// Method that injects a typed character event into the system. 
void cegui_sstm_injectChar(struct hg3dclass_struct * thisclass_c, int code_point_c, int * result_c);

// Method that injects a mouse-wheel / scroll-wheel event into the system. 
void cegui_sstm_injectMouseWheelChange(struct hg3dclass_struct * thisclass_c, float delta_c, int * result_c);

// Method that injects a new position for the mouse cursor. 
void cegui_sstm_injectMousePosition(struct hg3dclass_struct * thisclass_c, float x_pos_c, float y_pos_c, int * result_c);

// Method to inject time pulses into the system. 
void cegui_sstm_injectTimePulse(struct hg3dclass_struct * thisclass_c, float timeElapsed_c, int * result_c);

// Function to directly inject a mouse button click event. 
void cegui_sstm_injectMouseButtonClick(struct hg3dclass_struct * thisclass_c, const enum EnumMouseButton button_c, int * result_c);

// Function to directly inject a mouse button double-click event. 
void cegui_sstm_injectMouseButtonDoubleClick(struct hg3dclass_struct * thisclass_c, const enum EnumMouseButton button_c, int * result_c);

// Function to directly inject a mouse button triple-click event. 
void cegui_sstm_injectMouseButtonTripleClick(struct hg3dclass_struct * thisclass_c, const enum EnumMouseButton button_c, int * result_c);

#endif 
