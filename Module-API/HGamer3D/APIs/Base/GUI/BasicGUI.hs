-- This source file is part of HGamer3D
-- (A project to enable 3D game development in Haskell)
-- For the latest info, see http://www.althainz.de/HGamer3D.html
--
-- (c) 2011 Peter Althainz
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

-- BasicGUI.hs

-- |GUI functionality of the Base API.

module HGamer3D.APIs.Base.GUI.BasicGUI

(
        -- * Types
	GUIElement (..),
	
        -- * Initialization and configuration functions
	loadGuiLayoutFromFile,
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
	
        -- * Set specific GUI Element properties
	getGuiElProperty,
	setGuiElProperty,
	enableGuiEl,
	disableGuiEl,
	activateGuiEl,
	deactivateGuiEl,
	showGuiEl,
	hideGuiEl,
	
        -- * Listbox, Compobox specific functions
	listboxAddText,
	listboxGetSelectedText,
	listboxRemoveAllText,
	comboboxAddText,
	comboboxRemoveAllText,
	
        -- * GUI Event handling
	mapGuiElEventToFunction,
)

where

import GHC.Ptr

import HGamer3D.APIs.Base.Engine.Types
import HGamer3D.APIs.Base.Engine.Engine
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

import HGamer3D.Data.HG3DClass
import Control.Monad.Trans
import Control.Monad.Reader
import Control.Monad.Loops
import HGamer3D.Data.Vector

-- | The GUI Element, a window, a button, any widget or widget element, organized in a hierarchy
data GUIElement = GUIElement HG3DClass -- ^ only one constructor, currently GUI elements are not specified in more detail

-- | load a complete layout file into the application
loadGuiLayoutFromFile :: String -- ^ filename, without the path (this is defined by resource locations)
                         -> String -- ^ prefix to append to the identifiers in the layout, this allows, to load the same dailog/gui element multiple times with different names, if needed
                         -> MHGamer3D GUIElement -- ^ the resulting tree of GUI elements as top GUI element
loadGuiLayoutFromFile layoutFile prefix = do
	rs <- ask
	let gui = guiSystem rs
	let hg3dWinMgr = guiWindowManagerHG3D gui
	window <- liftIO $ WindowManagerHG3D.loadWindowLayoutHG3D hg3dWinMgr layoutFile prefix
	return (GUIElement window)

-- | add a GUI element to the display
addGuiElToDisplay :: GUIElement -- ^ GUI element
                     -> MHGamer3D ()
addGuiElToDisplay (GUIElement window) = do
	rs <- ask
	let gui = guiSystem rs
	let guiS = guiGUI gui
	guiSheet <- liftIO $ System.getGUISheet guiS
	liftIO $ Window.addChildWindow2 guiSheet window

-- | remove a GUI element from the display
removeGuiElFromDisplay :: GUIElement -- ^ GUI element
                          -> MHGamer3D ()
removeGuiElFromDisplay (GUIElement window) = do
	rs <- ask
	let gui = guiSystem rs
	let guiS = guiGUI gui
	guiSheet <- liftIO $ System.getGUISheet guiS
	liftIO $ Window.removeChildWindow2 guiSheet window

-- | enable GUI element, allow interaction with it and render it accordingly
enableGuiEl :: GUIElement -- ^ GUI element to enable
               -> MHGamer3D ()
enableGuiEl (GUIElement window) = do
	liftIO $ Window.enable window

-- | disable GUI element, disallow interaction with it and render it accordingly
disableGuiEl :: GUIElement -- ^ GUI element to disable
                -> MHGamer3D ()
disableGuiEl (GUIElement window) = do
	liftIO $ Window.disable window

-- | activate GUI element, process events send to it
activateGuiEl :: GUIElement -- ^ GUI element to activate
                 -> MHGamer3D ()
activateGuiEl (GUIElement window) = do
	liftIO $ Window.activate window

-- | deactivate GUI element, do not process events send to it
deactivateGuiEl :: GUIElement -- ^ GUI element to deactivate
                   -> MHGamer3D ()
deactivateGuiEl (GUIElement window) = do
	liftIO $ Window.deactivate window

-- | show GUI element
showGuiEl :: GUIElement -- ^ GUI element to show
             -> MHGamer3D ()
showGuiEl (GUIElement window) = do
	liftIO $ Window.show window

-- | hide GUI element
hideGuiEl :: GUIElement -- ^ GUI element to hide
             -> MHGamer3D ()
hideGuiEl (GUIElement window) = do
	liftIO $ Window.hide window

-- | get child gui element, throws an exception, if element not found in children, only searches the direct children of element.
getChildGuiEl :: GUIElement -- ^ GUI element, which childrens are searched for child element
                 -> String  -- ^ name of searched child element
                 -> MHGamer3D GUIElement -- ^ found element (throws exception, in case no child found)
getChildGuiEl (GUIElement window) name = do
	window <- liftIO $ Window.getChild window name
	return (GUIElement window)

-- | find child element recursively, searches all sub-trees
findChildGuiElRecursive :: GUIElement -- ^ GUI element, which childrens are searched, including children of children and so on.
                           -> String -- ^ name of child element to be found
                           -> MHGamer3D (Maybe GUIElement) -- ^ in case of found element: Just el, Nothing otherwhise
findChildGuiElRecursive (GUIElement window) name = do
	window <- liftIO $ Window.getChildRecursive window name
	if (ocPtr window) == nullPtr then do
		return Nothing
		else do
			return (Just (GUIElement window))

-- | get a named property as string from element
getGuiElProperty :: GUIElement -- ^ GUI element
                    -> String -- ^ property name to get
                    -> MHGamer3D String -- ^ property value
getGuiElProperty (GUIElement window) name = do
	prop <- liftIO $ PropertySet.getProperty window name
	return prop

-- | set a named property 
setGuiElProperty :: GUIElement -- ^ GUI element
                    -> String  -- ^ property name to set
                    -> String -- ^ property value as string
                    -> MHGamer3D ()
setGuiElProperty (GUIElement window) name value = do
	liftIO $ PropertySet.setProperty window name value

-- | load a complete GUI scheme, see CEGUI manual for details
loadGuiScheme :: String -- ^ scheme name
                 -> MHGamer3D ()
loadGuiScheme schemeName = do
	rs <- ask
	let gui = guiSystem rs
	liftIO $ SystemHG3D.schemeManagerCreate (guiSchemeManager gui) schemeName

-- | load a GUI font
loadGuiFont :: String -- ^ font name
               -> MHGamer3D ()	
loadGuiFont fontName = do
	rs <- ask
	let gui = guiSystem rs
	liftIO $ SystemHG3D.fontManagerCreate (guiFontManager gui) fontName
	
-- | set default GUI font
setGuiDefaultFont :: String -- ^ font name
                     -> MHGamer3D ()	
setGuiDefaultFont fontName = do
	rs <- ask
	let gui = guiSystem rs
	let guiS = guiGUI gui
	liftIO $ System.setDefaultFont guiS fontName
	
-- | set default mouse cursor
setGuiDefaultMouseCursor :: String -- ^ scheme name, where cursor is contained in
                            -> String -- ^ cursor name
                            -> MHGamer3D ()	
setGuiDefaultMouseCursor schemeName cursorName = do
	rs <- ask
	let gui = guiSystem rs
	let guiS = guiGUI gui
	liftIO $ System.setDefaultMouseCursor3 guiS schemeName cursorName

-- | set default tool tip
setGuiDefaultTooltip :: String -- ^ default tool tip name
                        -> MHGamer3D ()	
setGuiDefaultTooltip ttName = do
	rs <- ask
	let gui = guiSystem rs
	let guiS = guiGUI gui
	liftIO $ System.setDefaultTooltip2  guiS  ttName

-- | directly maps one specific event of one specific GUI element to a callback function
mapGuiElEventToFunction :: GUIElement -- ^ the GUI element from which the event is mapped
                           -> String -- ^ the event name, see the CEGUI documentation for details
                           -> EventFunction -- ^ an event function, the impact needs to be on a side effect
                           -> MHGamer3D ()
mapGuiElEventToFunction (GUIElement window) eventName function = do
	functionTag <- getUniqueName "Event"
	mapFunctionToTag functionTag function
	liftIO $ EvtSF.subscribeScriptedEvent window eventName functionTag
	return ()

-- | add one line of text as a selectable entry to a combobox
comboboxAddText :: GUIElement -- ^ GUI element, needs to be a combobox
                   -> String -- ^ the entry string to add
                   -> MHGamer3D ()
comboboxAddText (GUIElement window) itemname = do
	realcombo <- liftIO $ WindowSF.castWindowToCombobox window
	liftIO $ ListboxSF.comboboxAddItem realcombo itemname
	
-- | removes all lines of entries from a combobox
comboboxRemoveAllText :: GUIElement -- ^ the GUI elements, needs to be a combobox
                         -> MHGamer3D ()
comboboxRemoveAllText (GUIElement window) = do
	realcombo <- liftIO $ WindowSF.castWindowToCombobox window
	liftIO $ Combobox.resetList realcombo
	
-- | add one line of text as a selectable entry to a listbox
listboxAddText :: GUIElement -- ^ GUI element, needs to be a listbox
                  -> String -- ^ the entry string to add
                  -> MHGamer3D ()
listboxAddText (GUIElement window) itemname = do
	reallistbox <- liftIO $ WindowSF.castWindowToListbox window
	liftIO $ ListboxSF.listboxAddItem reallistbox itemname
	
_getTextListOfItem reallistbox item list = do
  let (HG3DClass ptra ptrb) = item
  if ptra /= nullPtr then do
    txt <- liftIO $ ListboxItem.getText item
    let list' = list ++ [txt]
    item' <- liftIO $ Listbox.getNextSelected reallistbox item
    list'' <- _getTextListOfItem reallistbox item' list'
    return list''
    else do
      return list
 
-- | return the selected items as an array of strings
listboxGetSelectedText :: GUIElement -- ^ the GUI element, needs to be a listbox
                          -> MHGamer3D [String] -- ^ the selected items as an array of strings
listboxGetSelectedText (GUIElement window) = do
	reallistbox <- liftIO $ WindowSF.castWindowToListbox window
	item <- liftIO $ Listbox.getFirstSelectedItem reallistbox
        list <-  _getTextListOfItem reallistbox item ([]::[String])
        return list
				
-- | removes all lines of entries from a listbox
listboxRemoveAllText :: GUIElement -- ^ the GUI element, needs to be a listbox
                        -> MHGamer3D ()
listboxRemoveAllText (GUIElement window) = do
	reallistbox <- liftIO $ WindowSF.castWindowToListbox window
	liftIO $ Listbox.resetList reallistbox
	
	
