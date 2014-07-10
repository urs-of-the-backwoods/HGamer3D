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

// ClassWindow.h

// 

#include "wchar.h"
#ifndef _DEFINED_HG3D_ClassWindow
#define _DEFINED_HG3D_ClassWindow

#include "ClassPtr.h"
#include "ClassFont.h"
#include "ClassTooltip.h"
#include "EnumVerticalAlignment.h"
#include "EnumHorizontalAlignment.h"
#include "ClassDragContainer.h"
#include "ClassUDim.h"
#include "ClassUVector2.h"
#include "EnumWindowUpdateMode.h"


// Constructor for Window
void cegui_wnd_construct(char * type_c, char * name_c, struct hg3dclass_struct * result_c);

// Destructor for Window
void cegui_wnd_destruct(struct hg3dclass_struct * thisclass_c);

// return a String object holding the type name for this Window
void cegui_wnd_getType(struct hg3dclass_struct * thisclass_c, char * result_c);

// return a String object holding the name of this Window
void cegui_wnd_getName(struct hg3dclass_struct * thisclass_c, char * result_c);

// returns whether or not this Window
void cegui_wnd_isDestroyedByParent(struct hg3dclass_struct * thisclass_c, int * result_c);

// returns whether or not this WindowWindow
void cegui_wnd_isAlwaysOnTop(struct hg3dclass_struct * thisclass_c, int * result_c);

// return whether the Window
void cegui_wnd_isDisabled(struct hg3dclass_struct * thisclass_c, int localOnly_c, int * result_c);

// return true if the Window
void cegui_wnd_isVisible(struct hg3dclass_struct * thisclass_c, int localOnly_c, int * result_c);

// return true if this is the active Window
void cegui_wnd_isActive(struct hg3dclass_struct * thisclass_c, int * result_c);

// return true if this WindowWindow
void cegui_wnd_isClippedByParent(struct hg3dclass_struct * thisclass_c, int * result_c);

// return the ID code currently assigned to this Window
void cegui_wnd_getID(struct hg3dclass_struct * thisclass_c, unsigned int * result_c);

// return the number of child WindowWindow
void cegui_wnd_getChildCount(struct hg3dclass_struct * thisclass_c, int * result_c);

// returns whether a WindowWindow
void cegui_wnd_isChild(struct hg3dclass_struct * thisclass_c, char * name_c, int * result_c);

// returns whether at least one window with the given ID code is attached to this Window
void cegui_wnd_isChild2(struct hg3dclass_struct * thisclass_c, unsigned int ID_c, int * result_c);

// returns whether at least one window with the given ID code is attached to this Window
void cegui_wnd_isChildRecursive(struct hg3dclass_struct * thisclass_c, unsigned int ID_c, int * result_c);

// return true if the given Window
void cegui_wnd_isChild3(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * window_c, int * result_c);

// return a pointer to the child window with the specified name. 
void cegui_wnd_getChild(struct hg3dclass_struct * thisclass_c, char * name_c, struct hg3dclass_struct * result_c);

// return a pointer to the first attached child window with the specified ID value. 
void cegui_wnd_getChild2(struct hg3dclass_struct * thisclass_c, unsigned int ID_c, struct hg3dclass_struct * result_c);

// return a pointer to the first attached child window with the specified name. Children are traversed recursively. 
void cegui_wnd_getChildRecursive(struct hg3dclass_struct * thisclass_c, char * name_c, struct hg3dclass_struct * result_c);

// return a pointer to the first attached child window with the specified ID value. Children are traversed recursively. 
void cegui_wnd_getChildRecursive2(struct hg3dclass_struct * thisclass_c, unsigned int ID_c, struct hg3dclass_struct * result_c);

// return a pointer to the child window that is attached to 'this' at the given index. 
void cegui_wnd_getChildAtIdx(struct hg3dclass_struct * thisclass_c, int idx_c, struct hg3dclass_struct * result_c);

// return a pointer to the WindowWindow
void cegui_wnd_getActiveChild(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * result_c);

// 
void cegui_wnd_getActiveChild2(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * result_c);

// return true if the specified WindowWindow
void cegui_wnd_isAncestor(struct hg3dclass_struct * thisclass_c, char * name_c, int * result_c);

// return true if any WindowWindow
void cegui_wnd_isAncestor2(struct hg3dclass_struct * thisclass_c, unsigned int ID_c, int * result_c);

// return true if the specified WindowWindow
void cegui_wnd_isAncestor3(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * window_c, int * result_c);

// return the active FontWindow
void cegui_wnd_getFont(struct hg3dclass_struct * thisclass_c, int useDefault_c, struct hg3dclass_struct * result_c);

// return the current text for the Window
void cegui_wnd_getText(struct hg3dclass_struct * thisclass_c, char * result_c);

// return text string with visual
void cegui_wnd_getTextVisual(struct hg3dclass_struct * thisclass_c, char * result_c);

// return true if the Window
void cegui_wnd_inheritsAlpha(struct hg3dclass_struct * thisclass_c, int * result_c);

// return the current alpha value set for this Window
void cegui_wnd_getAlpha(struct hg3dclass_struct * thisclass_c, float * result_c);

// return the effective alpha value that will be used when rendering this window, taking into account inheritance of parent window(s) alpha. 
void cegui_wnd_getEffectiveAlpha(struct hg3dclass_struct * thisclass_c, float * result_c);

// return true if this Window
void cegui_wnd_isCapturedByThis(struct hg3dclass_struct * thisclass_c, int * result_c);

// return true if an ancestor window has captured inputs. 
void cegui_wnd_isCapturedByAncestor(struct hg3dclass_struct * thisclass_c, int * result_c);

// return true if a child window has captured inputs. 
void cegui_wnd_isCapturedByChild(struct hg3dclass_struct * thisclass_c, int * result_c);

// return the parent of this Window
void cegui_wnd_getParent(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * result_c);

// Return whether this window is set to restore old input capture when it loses input capture. 
void cegui_wnd_restoresOldCapture(struct hg3dclass_struct * thisclass_c, int * result_c);

// Return whether z-order changes are enabled or disabled for this Window
void cegui_wnd_isZOrderingEnabled(struct hg3dclass_struct * thisclass_c, int * result_c);

// Return whether this window will receive multi-click events or multiple 'down' events instead. 
void cegui_wnd_wantsMultiClickEvents(struct hg3dclass_struct * thisclass_c, int * result_c);

// Return whether mouse button down event autorepeat is enabled for this window. 
void cegui_wnd_isMouseAutoRepeatEnabled(struct hg3dclass_struct * thisclass_c, int * result_c);

// Return the current auto-repeat delay setting for this window. 
void cegui_wnd_getAutoRepeatDelay(struct hg3dclass_struct * thisclass_c, float * result_c);

// Return the current auto-repeat rate setting for this window. 
void cegui_wnd_getAutoRepeatRate(struct hg3dclass_struct * thisclass_c, float * result_c);

// Return whether the window wants inputs passed to its attached child windows when the window has inputs captured. 
void cegui_wnd_distributesCapturedInputs(struct hg3dclass_struct * thisclass_c, int * result_c);

// Return whether this WindowTooltipTooltip
void cegui_wnd_isUsingDefaultTooltip(struct hg3dclass_struct * thisclass_c, int * result_c);

// Return a pointer to the TooltipWindowTooltipWindowTooltip
void cegui_wnd_getTooltip(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * result_c);

// Return the custom tooltip type. 
void cegui_wnd_getTooltipType(struct hg3dclass_struct * thisclass_c, char * result_c);

// Return the current tooltip text set for this Window
void cegui_wnd_getTooltipText(struct hg3dclass_struct * thisclass_c, char * result_c);

// Return whether this window inherits Tooltip
void cegui_wnd_inheritsTooltipText(struct hg3dclass_struct * thisclass_c, int * result_c);

// Return whether this window will rise to the top of the z-order when clicked with the left mouse button. 
void cegui_wnd_isRiseOnClickEnabled(struct hg3dclass_struct * thisclass_c, int * result_c);

// Return whether this window was inherited from the given class name at some point in the inheritance hierarchy. 
void cegui_wnd_testClassName(struct hg3dclass_struct * thisclass_c, char * class_name_c, int * result_c);

// Get the vertical alignment. 
void cegui_wnd_getVerticalAlignment(struct hg3dclass_struct * thisclass_c, enum EnumVerticalAlignment * result_c);

// Get the horizontal alignment. 
void cegui_wnd_getHorizontalAlignment(struct hg3dclass_struct * thisclass_c, enum EnumHorizontalAlignment * result_c);

// Get the name of the LookNFeel assigned to this window. 
void cegui_wnd_getLookNFeel(struct hg3dclass_struct * thisclass_c, char * result_c);

// Get whether or not this Window
void cegui_wnd_getModalState(struct hg3dclass_struct * thisclass_c, int * result_c);

// Returns a named user string. 
void cegui_wnd_getUserString(struct hg3dclass_struct * thisclass_c, char * name_c, char * result_c);

// Return whether a user string with the specified name exists. 
void cegui_wnd_isUserStringDefined(struct hg3dclass_struct * thisclass_c, char * name_c, int * result_c);

// Returns the active sibling window. 
void cegui_wnd_getActiveSibling(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * result_c);

// Return the pixel Width of the parent element. This always returns a valid number. 
void cegui_wnd_getParentPixelWidth(struct hg3dclass_struct * thisclass_c, float * result_c);

// Return the pixel Height of the parent element. This always returns a valid number. 
void cegui_wnd_getParentPixelHeight(struct hg3dclass_struct * thisclass_c, float * result_c);

// Returns whether this window should ignore mouse event and pass them through to and other windows behind it. In effect making the window transparent to the mouse. 
void cegui_wnd_isMousePassThroughEnabled(struct hg3dclass_struct * thisclass_c, int * result_c);

// Returns whether this window is an auto-child window. All auto-child windows have "__auto_" in their name, but this is faster. 
void cegui_wnd_isAutoWindow(struct hg3dclass_struct * thisclass_c, int * result_c);

// Returns whether this window is allowed to write XML. 
void cegui_wnd_isWritingXMLAllowed(struct hg3dclass_struct * thisclass_c, int * result_c);

// Returns whether this Window
void cegui_wnd_isDragDropTarget(struct hg3dclass_struct * thisclass_c, int * result_c);

// Returns whether automatic
void cegui_wnd_isUsingAutoRenderingSurface(struct hg3dclass_struct * thisclass_c, int * result_c);

// Returns the window at the root of the hierarchy starting at this Window
void cegui_wnd_getRootWindow(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * result_c);

// 
void cegui_wnd_getRootWindow2(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * result_c);

// Return whether the Window
void cegui_wnd_isNonClientWindow(struct hg3dclass_struct * thisclass_c, int * result_c);

// Renames the window. 
void cegui_wnd_rename(struct hg3dclass_struct * thisclass_c, char * new_name_c);

// Initialises the Window
void cegui_wnd_initialiseComponents(struct hg3dclass_struct * thisclass_c);

// Set whether or not this WindowWindow
void cegui_wnd_setDestroyedByParent(struct hg3dclass_struct * thisclass_c, int setting_c);

// Set whether this window is always on top, or not. 
void cegui_wnd_setAlwaysOnTop(struct hg3dclass_struct * thisclass_c, int setting_c);

// Set whether this window is enabled or disabled. A disabled window normally can not be interacted with, and may have different rendering. 
void cegui_wnd_setEnabled(struct hg3dclass_struct * thisclass_c, int setting_c);

// enable the Window
void cegui_wnd_enable(struct hg3dclass_struct * thisclass_c);

// disable the Window
void cegui_wnd_disable(struct hg3dclass_struct * thisclass_c);

// Set whether the Window
void cegui_wnd_setVisible(struct hg3dclass_struct * thisclass_c, int setting_c);

// show the Window
void cegui_wnd_show(struct hg3dclass_struct * thisclass_c);

// hide the Window
void cegui_wnd_hide(struct hg3dclass_struct * thisclass_c);

// Activate the WindowWindow
void cegui_wnd_activate(struct hg3dclass_struct * thisclass_c);

// Deactivate the window. No further inputs will be received by the window until it is re-activated either programmatically or by the user interacting with the gui. 
void cegui_wnd_deactivate(struct hg3dclass_struct * thisclass_c);

// Set whether this Window
void cegui_wnd_setClippedByParent(struct hg3dclass_struct * thisclass_c, int setting_c);

// Set the current ID for the Window
void cegui_wnd_setID(struct hg3dclass_struct * thisclass_c, unsigned int ID_c);

// Set the current text string for the Window
void cegui_wnd_setText(struct hg3dclass_struct * thisclass_c, char * text_c);

// Append the string textWindow
void cegui_wnd_appendText(struct hg3dclass_struct * thisclass_c, char * text_c);

// Set the font used by this Window
void cegui_wnd_setFont(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * font_c);

// Set the font used by this Window
void cegui_wnd_setFont2(struct hg3dclass_struct * thisclass_c, char * name_c);

// Add the named WindowWindowWindownameWindowWindow
void cegui_wnd_addChildWindow(struct hg3dclass_struct * thisclass_c, char * name_c);

// Add the specified WindowWindowWindowwindowWindowWindow
void cegui_wnd_addChildWindow2(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * window_c);

// Remove the named Window
void cegui_wnd_removeChildWindow(struct hg3dclass_struct * thisclass_c, char * name_c);

// Remove the specified Window
void cegui_wnd_removeChildWindow2(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * window_c);

// Remove the first child WindowWindow
void cegui_wnd_removeChildWindow3(struct hg3dclass_struct * thisclass_c, unsigned int ID_c);

// Move the Window
void cegui_wnd_moveToFront(struct hg3dclass_struct * thisclass_c);

// Move the Window
void cegui_wnd_moveToBack(struct hg3dclass_struct * thisclass_c);

// Captures input to this window. 
void cegui_wnd_captureInput(struct hg3dclass_struct * thisclass_c, int * result_c);

// Releases input capture from this WindowWindow
void cegui_wnd_releaseInput(struct hg3dclass_struct * thisclass_c);

// Set whether this window will remember and restore the previous window that had inputs captured. 
void cegui_wnd_setRestoreCapture(struct hg3dclass_struct * thisclass_c, int setting_c);

// Set the current alpha value for this window. 
void cegui_wnd_setAlpha(struct hg3dclass_struct * thisclass_c, float alpha_c);

// Sets whether this Window
void cegui_wnd_setInheritsAlpha(struct hg3dclass_struct * thisclass_c, int setting_c);

// Invalidate this window causing at least this window to be redrawn during the next rendering pass. 
void cegui_wnd_invalidate(struct hg3dclass_struct * thisclass_c);

// Invalidate this window and - dependant upon recursive
void cegui_wnd_invalidate2(struct hg3dclass_struct * thisclass_c, const int recursive_c);

// Set the mouse cursor image to be used when the mouse enters this window. 
void cegui_wnd_setMouseCursor3(struct hg3dclass_struct * thisclass_c, char * imageset_c, char * image_name_c);

// Set whether z-order changes are enabled or disabled for this Window
void cegui_wnd_setZOrderingEnabled(struct hg3dclass_struct * thisclass_c, int setting_c);

// Set whether this window will receive multi-click events or multiple 'down' events instead. 
void cegui_wnd_setWantsMultiClickEvents(struct hg3dclass_struct * thisclass_c, int setting_c);

// Set whether mouse button down event autorepeat is enabled for this window. 
void cegui_wnd_setMouseAutoRepeatEnabled(struct hg3dclass_struct * thisclass_c, int setting_c);

// Set the current auto-repeat delay setting for this window. 
void cegui_wnd_setAutoRepeatDelay(struct hg3dclass_struct * thisclass_c, float delay_c);

// Set the current auto-repeat rate setting for this window. 
void cegui_wnd_setAutoRepeatRate(struct hg3dclass_struct * thisclass_c, float rate_c);

// Set whether the window wants inputs passed to its attached child windows when the window has inputs captured. 
void cegui_wnd_setDistributesCapturedInputs(struct hg3dclass_struct * thisclass_c, int setting_c);

// Internal support method for drag & drop. You do not normally call this directly from client code. See the DragContainer
void cegui_wnd_notifyDragDropItemEnters(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * item_c);

// Internal support method for drag & drop. You do not normally call this directly from client code. See the DragContainer
void cegui_wnd_notifyDragDropItemLeaves(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * item_c);

// Internal support method for drag & drop. You do not normally call this directly from client code. See the DragContainer
void cegui_wnd_notifyDragDropItemDropped(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * item_c);

// Internal destroy method which actually just adds the window and any parent destructed child windows to the dead pool. 
void cegui_wnd_destroy(struct hg3dclass_struct * thisclass_c);

// Set the custom TooltipWindowWindowTooltip
void cegui_wnd_setTooltip(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * tooltip_c);

// Set the custom TooltipWindowWindow
void cegui_wnd_setTooltipType(struct hg3dclass_struct * thisclass_c, char * tooltipType_c);

// Set the tooltip text for this window. 
void cegui_wnd_setTooltipText(struct hg3dclass_struct * thisclass_c, char * tip_c);

// Set whether this window inherits Tooltip
void cegui_wnd_setInheritsTooltipText(struct hg3dclass_struct * thisclass_c, int setting_c);

// Set whether this window will rise to the top of the z-order when clicked with the left mouse button. 
void cegui_wnd_setRiseOnClickEnabled(struct hg3dclass_struct * thisclass_c, int setting_c);

// Set the vertical alignment. 
void cegui_wnd_setVerticalAlignment(struct hg3dclass_struct * thisclass_c, const enum EnumVerticalAlignment alignment_c);

// Set the horizontal alignment. 
void cegui_wnd_setHorizontalAlignment(struct hg3dclass_struct * thisclass_c, const enum EnumHorizontalAlignment alignment_c);

// Set the LookNFeel that shoule be used for this window. 
void cegui_wnd_setLookNFeel(struct hg3dclass_struct * thisclass_c, char * look_c);

// Set the modal state for this Window
void cegui_wnd_setModalState(struct hg3dclass_struct * thisclass_c, int state_c);

// method called to perform extended laying out of attached child windows. 
void cegui_wnd_performChildWindowLayout(struct hg3dclass_struct * thisclass_c);

// Sets the value a named user string, creating it as required. 
void cegui_wnd_setUserString(struct hg3dclass_struct * thisclass_c, char * name_c, char * value_c);

// Set the window area. 
void cegui_wnd_setArea(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * xpos_c, struct hg3dclass_struct * ypos_c, struct hg3dclass_struct * width_c, struct hg3dclass_struct * height_c);

// Set the window area. 
void cegui_wnd_setArea2(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * pos_c, struct hg3dclass_struct * size_c);

// Set the window's position. 
void cegui_wnd_setPosition(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * pos_c);

// Set the window's X position. 
void cegui_wnd_setXPosition(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * x_c);

// Set the window's Y position. 
void cegui_wnd_setYPosition(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * y_c);

// Set the window's size. 
void cegui_wnd_setSize(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * size_c);

// Set the window's width. 
void cegui_wnd_setWidth(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * width_c);

// Set the window's height. 
void cegui_wnd_setHeight(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * height_c);

// Set the window's maximum size. 
void cegui_wnd_setMaxSize(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * size_c);

// Set the window's minimum size. 
void cegui_wnd_setMinSize(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * size_c);

// Get the window's position. 
void cegui_wnd_getPosition(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * result_c);

// Get the window's X position. 
void cegui_wnd_getXPosition(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * result_c);

// Get the window's Y position. 
void cegui_wnd_getYPosition(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * result_c);

// Get the window's maximum size. 
void cegui_wnd_getMaxSize(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * result_c);

// Get the window's minimum size. 
void cegui_wnd_getMinSize(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * result_c);

// Causes the Window
void cegui_wnd_render(struct hg3dclass_struct * thisclass_c);

// Cause window to update itself and any attached children. Client code does not need to call this method; to ensure full, and proper updates, call the injectTimePulse methodname method provided by the System
void cegui_wnd_update(struct hg3dclass_struct * thisclass_c, float elapsed_c);

// Sets the internal 'initialising' flag to true. This can be use to optimize initialisation of some widgets, and is called automatically by the layout XML handler when it has created a window. That is just after the window has been created, but before any children or properties are read. 
void cegui_wnd_beginInitialisation(struct hg3dclass_struct * thisclass_c);

// Sets the internal 'initialising' flag to false. This is called automatically by the layout XML handler when it is done creating a window. That is after all properties and children have been loaded and just before the next sibling gets created. 
void cegui_wnd_endInitialisation(struct hg3dclass_struct * thisclass_c);

// Sets whether this window should ignore mouse events and pass them through to any windows behind it. In effect making the window transparent to the mouse. 
void cegui_wnd_setMousePassThroughEnabled(struct hg3dclass_struct * thisclass_c, int setting_c);

// Assign the WindowRenderer to specify the Look'N'Feel specification to be used. 
void cegui_wnd_setWindowRenderer(struct hg3dclass_struct * thisclass_c, char * name_c);

// Get the factory name of the currently assigned WindowRenderer. (Look'N'Feel specification). 
void cegui_wnd_getWindowRendererName(struct hg3dclass_struct * thisclass_c, char * result_c);

// Sets whether this window is allowed to write XML. 
void cegui_wnd_setWritingXMLAllowed(struct hg3dclass_struct * thisclass_c, int allow_c);

// Inform the window, and optionally all children, that screen area rectangles have changed. 
void cegui_wnd_notifyScreenAreaChanged(struct hg3dclass_struct * thisclass_c, int recursive_c);

// Changes the widget's falagard type, thus changing its look'n'feel and optionally its renderer in the process. 
void cegui_wnd_setFalagardType(struct hg3dclass_struct * thisclass_c, char * type_c, char * rendererType_c);

// Specifies whether this Window
void cegui_wnd_setDragDropTarget(struct hg3dclass_struct * thisclass_c, int setting_c);

// Invalidate the chain of rendering surfaces from this window backwards to ensure they get properly redrawn - but doing the minimum amount of work possibe - next render. 
void cegui_wnd_invalidateRenderingSurface(struct hg3dclass_struct * thisclass_c);

// Sets whether automatic
void cegui_wnd_setUsingAutoRenderingSurface(struct hg3dclass_struct * thisclass_c, int setting_c);

// Set whether the Window
void cegui_wnd_setNonClientWindow(struct hg3dclass_struct * thisclass_c, const int setting_c);

// return whether text parsing is enabled for this window. 
void cegui_wnd_isTextParsingEnabled(struct hg3dclass_struct * thisclass_c, int * result_c);

// set whether text parsing is enabled for this window. 
void cegui_wnd_setTextParsingEnabled(struct hg3dclass_struct * thisclass_c, const int setting_c);

// Add the named property to the XML ban list for this window. 
void cegui_wnd_banPropertyFromXML(struct hg3dclass_struct * thisclass_c, char * property_name_c);

// Remove the named property from the XML ban list for this window. 
void cegui_wnd_unbanPropertyFromXML(struct hg3dclass_struct * thisclass_c, char * property_name_c);

// Return whether the named property is banned from XML. 
void cegui_wnd_isPropertyBannedFromXML(struct hg3dclass_struct * thisclass_c, char * property_name_c, int * result_c);

// Set the window update mode. This mode controls the behaviour of the Window::update
void cegui_wnd_setUpdateMode(struct hg3dclass_struct * thisclass_c, const enum EnumWindowUpdateMode mode_c);

// Return the current window update mode that is set for this WindowWindow::update
void cegui_wnd_getUpdateMode(struct hg3dclass_struct * thisclass_c, enum EnumWindowUpdateMode * result_c);

// Set whether mouse input that is not directly handled by this WindowWindow
void cegui_wnd_setMouseInputPropagationEnabled(struct hg3dclass_struct * thisclass_c, const int enabled_c);

// Return whether mouse input that is not directly handled by this WindowWindow
void cegui_wnd_isMouseInputPropagationEnabled(struct hg3dclass_struct * thisclass_c, int * result_c);

// Clones this Window
void cegui_wnd_clone(struct hg3dclass_struct * thisclass_c, char * newName_c, const int deepCopy_c, struct hg3dclass_struct * result_c);

// copies this widget's properties to given target widget 
void cegui_wnd_clonePropertiesTo(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * target_c);

// copies this widget's child widgets to given target widget 
void cegui_wnd_cloneChildWidgetsTo(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * target_c);

// Return the (visual) z index of the window on it's parent. 
void cegui_wnd_getZIndex(struct hg3dclass_struct * thisclass_c, int * result_c);

// Return whether /a this Window
void cegui_wnd_isInFront(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * wnd_c, int * result_c);

// Return whether /a this Window
void cegui_wnd_isBehind(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * wnd_c, int * result_c);

// return the Window
void cegui_wnd_getCaptureWindow(struct hg3dclass_struct * result_c);

#endif 
