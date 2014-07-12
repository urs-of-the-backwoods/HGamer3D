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

-- |GUI functionality of HGamer3D, public API. 

module HGamer3D.GUI.BaseAPI

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

import HGamer3D.GUI.Internal.Base
import HGamer3D.GUI.Internal.Widgets
import HGamer3D.GUI.Internal.Properties

