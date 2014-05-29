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

-- GUI/Internal/Widgets.hs

-- |Widget functionality of the GUI. Implementation module with internal data structures exposed. Public API in HGamer3D.GUI.

module HGamer3D.GUI.Internal.Widgets

(
  
        GUIButton (..),
        GUIRadioButton (..),
        GUICheckBox (..),
        GUIEditText (..), 
        GUIListBox (..),
        GUIComboBox (..),
        GUISlider (..),
        GUISpinner (..),

        GEButton (..),
        GERadioButton (..),
        GECheckBox (..),
        GEEditText (..),
        GEListBox (..),
        GEComboBox (..),
        GESlider (..),
        GESpinner (..),

        typeOfGuiEl,
        toGuiType,

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
	
        -- * Listbox, Compobox specific functions
	listboxAddText,
	listboxGetSelectedText,
	listboxRemoveAllText,
        listboxInitialize,
        listboxStatus,
	comboboxAddText,
	comboboxRemoveAllText,
        
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
import Data.List.Split

import HGamer3D.Data
import HGamer3D.Util

-- import HGamer3D.Internal.WinEvent

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
import HGamer3D.Data.HG3DClass

data GEButton = GEButton
data GERadioButton = GERadioButton
data GECheckBox = GECheckBox

data GEEditText = GEEditText

data GEListBox = GEListBox
data GEComboBox = GEComboBox

data GESlider = GESlider
data GESpinner = GESpinner

-- | GUI Element, Sybtype Button
type GUIButton = GUIElement GEButton

-- | GUI Element, Sybtype RadioButton
type GUIRadioButton = GUIElement GERadioButton

-- | GUI Element, Sybtype CheckBox
type GUICheckBox = GUIElement GECheckBox

-- | GUI Element, Sybtype EditText
type GUIEditText = GUIElement GEEditText

-- | GUI Element, Sybtype Listbox
type GUIListBox = GUIElement GEListBox

-- | GUI Element, Sybtype Combobox
type GUIComboBox = GUIElement GEComboBox

-- | GUI Element, Sybtype Slider
type GUISlider = GUIElement GESlider

-- | GUI Element, Sybtype Spinner
type GUISpinner = GUIElement GESpinner


-- | get Type of GUI element as string
typeOfGuiEl :: GUIElement a -- ^ GUI element to enable
               -> IO String
typeOfGuiEl (GUIElement window _) = do
	Window.getType window

toGuiType :: String -> b -> GUIElement a -> IO (Maybe (GUIElement b))
toGuiType typestr cons guiel@(GUIElement window _) = do
          tp <- typeOfGuiEl guiel
          -- CEGUI Types as String have the format "WindowLook/Button" for example
          let tp' = (splitOn "/" tp) !! 1  
          let guiel' = case tp' of
                         typestr -> Just $ GUIElement window cons
                         _ -> Nothing
          return guiel'

toButton = toGuiType "Button" GEButton
toRadioButton = toGuiType "RadioButton" GERadioButton
toCheckBox = toGuiType "Checkbox" GECheckBox

toEditText = toGuiType "EditText" GEEditText

toComboBox = toGuiType "Combobox" GEComboBox
toListBox = toGuiType "Listbox" GEListBox

toSlider = toGuiType "Slider" GESlider
toSpinner = toGuiType "Spinner" GESpinner

findElement :: (GUIElement a -> IO (Maybe (GUIElement b))) 
               ->String 
               -> GUIElement a 
               -> IO (Maybe (GUIElement b))
findElement toNewType name topel = do
  mFound <- findChildGuiElRecursive topel name
  case mFound of
    Nothing -> return Nothing
    Just guiel -> toNewType guiel

findButton = findElement toButton
findRadioButton = findElement toRadioButton
findCheckBox = findElement toCheckBox
findEditText = findElement toEditText
findComboBox = findElement toComboBox
findListBox = findElement toListBox
findSpinner = findElement toSpinner
findSlider = findElement toSlider

-- | add one line of text as a selectable entry to a combobox
comboboxAddText :: GUIComboBox -- ^ GUI element, needs to be a combobox
                   -> String -- ^ the entry string to add
                   -> IO ()
comboboxAddText (GUIElement window GEComboBox) itemname = do
	realcombo <- WindowSF.castWindowToCombobox window
	ListboxSF.comboboxAddItem realcombo itemname
	
-- | removes all lines of entries from a combobox
comboboxRemoveAllText :: GUIComboBox -- ^ the GUI elements, needs to be a combobox
                         -> IO ()
comboboxRemoveAllText (GUIElement window GEComboBox) = do
	realcombo <- WindowSF.castWindowToCombobox window
	Combobox.resetList realcombo
	
-- | add one line of text as a selectable entry to a listbox
listboxAddText :: GUIListBox -- ^ GUI element, needs to be a listbox
                  -> String -- ^ the entry string to add
                  -> IO ()
listboxAddText (GUIElement window GEListBox) itemname = do
	reallistbox <- WindowSF.castWindowToListbox window
	ListboxSF.listboxAddItem reallistbox itemname
	
_getTextListOfItem reallistbox item list = do
  let (HG3DClass ptra ptrb) = item
  if ptra /= nullPtr then do
    txt <- ListboxItem.getText item
    let list' = list ++ [txt]
    item' <- Listbox.getNextSelected reallistbox item
    list'' <- _getTextListOfItem reallistbox item' list'
    return list''
    else do
      return list
 
listboxInitialize :: GUIListBox -- ^ GUI element, needs to be a listbox
                         -> [(String, Bool)] -- ^ list of entry, selected pairs
                         -> IO ()
listboxInitialize (GUIElement window GEListBox) pairs = do
	reallistbox <- WindowSF.castWindowToListbox window
	Listbox.resetList reallistbox
        mapM ( \((entry, selected), ind) -> do
                  	ListboxSF.listboxAddItem reallistbox entry
                        item <- Listbox.getListboxItemFromIndex reallistbox ind
                        ListboxItem.setSelected item selected
                        return ()
                  ) (zip pairs [0..])
        return ()
  
listboxStatus :: GUIListBox -- ^ GUI element, needs to be a listbox
                         -> IO [(String, Bool)] -- ^ list of entry, selected pairs
listboxStatus (GUIElement window GEListBox) = do
	reallistbox <- WindowSF.castWindowToListbox window
        count <- Listbox.getItemCount reallistbox
        outlist <- mapM ( \ind -> do
                        item <- Listbox.getListboxItemFromIndex reallistbox ind
                        sel <- ListboxItem.isSelected item
                        txt <- ListboxItem.getText item
                        return (txt, sel)
                  ) [0..(count-1)]
        return outlist
  

-- | return the selected items as an array of strings
listboxGetSelectedText :: GUIListBox -- ^ the GUI element, needs to be a listbox
                          -> IO [String] -- ^ the selected items as an array of strings
listboxGetSelectedText (GUIElement window GEListBox) = do
	reallistbox <- WindowSF.castWindowToListbox window
	item <- Listbox.getFirstSelectedItem reallistbox
        list <- _getTextListOfItem reallistbox item ([]::[String])
        return list
				
-- | removes all lines of entries from a listbox
listboxRemoveAllText :: GUIListBox -- ^ the GUI element, needs to be a listbox
                        -> IO ()
listboxRemoveAllText (GUIElement window GEListBox) = do
	reallistbox <- WindowSF.castWindowToListbox window
	Listbox.resetList reallistbox
	
