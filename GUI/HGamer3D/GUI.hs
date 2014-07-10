-- This source file is part of HGamer3D
-- (A project to enable 3D game development in Haskell)
-- For the latest info, see http://www.hgamer3d.org
--
-- (c) 2011-2013 Peter Althainz
-- 
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- You may obtain a copy of the License at
--
--     http://www.apache.org/licenses/LICENSE-2.0
--
-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS,
-- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
-- See the License for the specific language governing permissions and
-- limitations under the License.

-- GUI.hs

-- |GUI functionality of HGamer3D, public API. Currently not usefule standalone, but used inside HGamer3D (main module).

module HGamer3D.GUI

(
        -- * Data Definitions, Types
  
        GUISystem,
        GUIElement,
        GUIEvent (..),
        EventFunction,

        GUIDim (..),
        GUIVec2 (..),
        
        -- initGUI,
        -- freeGUI,
        -- pollGUIEvents,

        
        -- injectWinEventToGUI,

        -- * Event Handling
        registerGUIEvent,

        -- * Configuration: Font, Scheme, MouseCursor, Tooltip

	loadGuiScheme,
	loadGuiFont,
	setGuiDefaultFont,
	setGuiDefaultMouseCursor,
	setGuiDefaultTooltip,
	
        -- * Find and display GUI Elements
	getChildGuiEl,
	findChildGuiElRecursive,
        addGuiElToDisplay,
	removeGuiElFromDisplay,
	
        -- * Change Gui Element Tree
        addChildGuiEl,
        removeChildGuiEl,

        -- * Set specific GUI Element properties
	getGuiElProperty,
	setGuiElProperty,
	enableGuiEl,
	disableGuiEl,
	activateGuiEl,
	deactivateGuiEl,
	showGuiEl,
	hideGuiEl,
	
        -- * Widget Types and Conversions

        GUIButton,
        GUIRadioButton,
        GUICheckBox,
        GUIEditText, 
        GUIListBox,
        GUIComboBox,
        GUISlider,
        GUISpinner,

        -- * Create Widgets programmatically

        button,
        radioButton,
        checkBox,
        editText,
        comboBox,
        listBox,
        spinner,
        slider,

        -- * Load Widgets from layout file

	loadGuiLayoutFromFile,

        toButton,
        toRadioButton,
        toCheckBox,
        toEditText,
        toComboBox,
        toListBox,
        toSpinner, 
        toSlider,
	
        findButton,
        findRadioButton,
        findCheckBox,
        findEditText,
        findComboBox,
        findListBox,
        findSpinner,
        findSlider,
	
        -- * Widget Functions

	listboxAddText,
	listboxGetSelectedText,
	listboxRemoveAllText,
        listboxInitialize,
        listboxStatus,

	comboboxAddText,
	comboboxRemoveAllText,
        
 -- * Property Types

GUIElementProperty, 
GUIButtonProperty,
GUIRadioButtonProperty,
GUIHasSelectionProperty, 
GUICheckBoxProperty,
GUIEditTextProperty,
GUIHasValueProperty,
GUISliderProperty,
GUISpinnerProperty,

-- * Property Functions
(=:),
setP,
getP,

-- * GUI Widget General Properties

pText,
pDisabled,
pVisible,
pAlpha,
pTooltip,
pAlwaysOnTop,

pX,
pY,
pWidth,
pHeight,

pSelected,
pValue,


)

where

import HGamer3D.Bindings.CEGUI.ClassOgreRenderer as CEGUIOgreRenderer
import HGamer3D.Bindings.CEGUI.ClassSystem as CEGUISystem
import HGamer3D.Bindings.CEGUI.ClassSystemHG3D as CEGUISystemHG3D
import HGamer3D.Bindings.CEGUI.ClassWindowManagerHG3D as CEGUIWindowManagerHG3D
import HGamer3D.Bindings.CEGUI.ClassWindowManager as CEGUIWindowManager
import HGamer3D.Bindings.CEGUI.ClassResourceProvider as CEGUIResourceProvider
import HGamer3D.Bindings.CEGUI.ClassDefaultResourceProvider as CEGUIDefaultResourceProvider
import HGamer3D.Bindings.CEGUI.ClassImageset as CEGUIImageset
import HGamer3D.Bindings.CEGUI.ClassFont as CEGUIFont
import HGamer3D.Bindings.CEGUI.ClassScheme as CEGUIScheme
import HGamer3D.Bindings.CEGUI.ClassWidgetLookManager as CEGUIWidgetLookManager
import HGamer3D.Bindings.CEGUI.ClassScriptModule as CEGUIScriptModule
import HGamer3D.Bindings.CEGUI.ClassLogger as CEGUILogger
import HGamer3D.Bindings.CEGUI.EnumLoggingLevel
import HGamer3D.Bindings.CEGUI.EnumMouseButton as CEGUIButton
import HGamer3D.Bindings.CEGUI.ClassFontManager as CEGUIFontManager
import HGamer3D.Bindings.CEGUI.ClassSchemeManager as CEGUISchemeManager
import HGamer3D.Bindings.CEGUI.ClassDefaultLogger as CEGUIDefaultLogger
import HGamer3D.Bindings.CEGUI.ClassHG3DEventController as HG3DEventController
import HGamer3D.Bindings.CEGUI.ClassWindowManagerHG3D as HG3DWindowManager
--import HGamer3D.Bindings.Ogre.ClassHG3DMessagePump as MessagePump


import GHC.Ptr

import HGamer3D.Data
import HGamer3D.Util
import HGamer3D.WinEvent

import HGamer3D.Bindings.CEGUI.ClassPtr
import HGamer3D.Bindings.CEGUI.Utils

import HGamer3D.Bindings.CEGUI.ClassWindow as Window
import HGamer3D.Bindings.CEGUI.ClassWindowManager as WindowManager
import HGamer3D.Bindings.CEGUI.ClassWindowManagerHG3D as WindowManagerHG3D
import HGamer3D.Bindings.CEGUI.ClassWindow as Window
import HGamer3D.Bindings.CEGUI.ClassSystem as System
import HGamer3D.Bindings.CEGUI.ClassSystemHG3D as SystemHG3D
import HGamer3D.Bindings.CEGUI.ClassPropertySet as PropertySet
import HGamer3D.Bindings.CEGUI.ClassHG3DEventStaticFunctions as EvtSF 
import HGamer3D.Bindings.CEGUI.ClassHG3DListboxStaticFunctions as ListboxSF
import HGamer3D.Bindings.CEGUI.ClassHG3DWindowStaticFunctions as WindowSF
import HGamer3D.Bindings.CEGUI.ClassListbox as Listbox
import HGamer3D.Bindings.CEGUI.ClassListboxItem as ListboxItem
import HGamer3D.Bindings.CEGUI.ClassCombobox as Combobox
import HGamer3D.Bindings.CEGUI.EnumKeyScan

import HGamer3D.Bindings.CEGUI.ClassHG3DEventStaticFunctions as EvtSF 

import HGamer3D.Bindings.CEGUI.EnumMouseButton as CEGUIButton

import HGamer3D.GUI.Internal.Base
import HGamer3D.GUI.Internal.Widgets
import HGamer3D.GUI.Internal.Properties

