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
  -- * General GUI Functionality
  
        -- ** Data Definitions, Types
  
        GUISystem,

        -- ** Initializing
        
        initGUI,
        freeGUI,
        pollGUIEvent,

        -- ** Configuration
        
	loadGuiScheme,
	loadGuiFont,
	setGuiDefaultFont,
	setGuiDefaultMouseCursor,
	setGuiDefaultTooltip,

  -- * General Handling of GUI Elements

        -- ** Data Definition, Types
        
        GUIElement,
        GUIDim (..),
        GUIVec2 (..),

        -- ** Work on the GUI Element Tree

        addChildGuiEl,
	getChildGuiEl,
        removeChildGuiEl,
	findChildGuiElRecursive,

        -- ** Add Elements to Display and Remove from Display
        
        addGuiElToDisplay,
	removeGuiElFromDisplay,
	
        -- ** Work with GUI Element Properties
        
	getGuiElProperty,
	setGuiElProperty,
	enableGuiEl,
	disableGuiEl,
	activateGuiEl,
	deactivateGuiEl,
	showGuiEl,
	hideGuiEl,

        -- ** Work with File Layouts

        loadGuiLayoutFromFile,

 -- * Event Handling
        
        injectWinEventToGUI,
        injectGUITimeDelta,
        GUIEvent (..),
        registerGUIEvent,

-- * Widget Properties

 -- ** Property Types

        GUIElementProperty, 
        GUIHasValueProperty,

-- ** Property Functions
        (=:),
        setP,
        getP,

-- ** GUI Widget General Properties

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
        

-- * Widget Specific Types, Functions and Properties

        -- ** Button Widget
        GUIButton,
        GUIButtonProperty,
        button,
        toButton,
        findButton,

        -- ** RadioButton Widget
        GUIRadioButton,
        GUIRadioButtonProperty,
        radioButton,
        toRadioButton,
        findRadioButton,

        -- ** CheckBox Widget
        GUICheckBox,
        GUICheckBoxProperty,
        checkBox,
        toCheckBox,
        findCheckBox,

        -- ** EditText Widget
        GUIEditText, 
        GUIEditTextProperty,
        editText,
        toEditText,
        findEditText,

        -- ** Listbox/Combobox Widget        
        GUIListBox,
        GUIComboBox,
        GUIHasSelectionProperty, 
        comboBox,
        listBox,
        toComboBox,
        toListBox,
        findComboBox,
        findListBox,

        listboxAddText,
	listboxGetSelectedText,
	listboxRemoveAllText,
        listboxInitialize,
        listboxStatus,

	comboboxAddText,
	comboboxRemoveAllText,

        -- ** Slider/Spinner Widget
        GUISlider,
        GUISpinner,
        GUISliderProperty,
        GUISpinnerProperty,
        slider,
        spinner,
        toSlider,
        toSpinner, 
        findSlider,
        findSpinner,

        -- ** Layout Container Widgets
        GUIHLayout,
        GUIVLayout,
        GUIGridLayout,

        hLayout,
        vLayout,
        gridLayout,

        toHLayout,
        toVLayout,
        toGridLayout,

        -- ** CRUD for Form based GUIs
        GUIEngineData,
        createForm,
        updateForm,
        removeForm,
)

where

import HGamer3D.GUI.Internal.Base
import HGamer3D.GUI.Internal.Widgets
import HGamer3D.GUI.Internal.Properties
import HGamer3D.GUI.Internal.Form
import HGamer3D.GUI.Schema.GUIDim
