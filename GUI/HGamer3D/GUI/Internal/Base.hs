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

-- GUI/Internal/Base.hs

-- |Base functionality of the GUI. Implementation module with internal data structures exposed. Public API in HGamer3D.GUI.

module HGamer3D.GUI.Internal.Base

(
  
        -- * Data Definitions, Types
  
        GUISystem (..),
        GUIElement (..), 

        GUIEvent (..),
        EventFunction,
        
        -- * Initialization, Game Loop
        initGUI,
        freeGUI,
        pollGUIEvents,

        -- * Event Handling
        registerGUIEvent,
        injectWinEventToGUI,

        -- * Layout, Scheme, Font, Mause, Tooltip
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

import HGamer3D.Data.HG3DClass

import HGamer3D.WinEvent.Internal.Base (showCursor)

data GUISystem = GUISystem {
    guiRoot::HG3DClass,
    guiRenderer::HG3DClass,
    guiWindowManager::HG3DClass,
    guiWindowManagerHG3D::HG3DClass,
    guiFontManager::HG3DClass,
    guiSchemeManager::HG3DClass,
    guiEventController::HG3DClass,
    guiUniqueName::UniqueName
} 

type EventFunction = IO ()

-- | The GUI Element, a window, a button, any widget or widget element, organized in a hierarchy
data GUIElement a = GUIElement HG3DClass a -- ^ only one constructor, phantom type a, to allow GUIElement sub-typing




-- | load a complete layout file into the application
loadGuiLayoutFromFile ::  GUISystem 
                         -> String -- ^ filename, without the path (this is defined by resource locations)
                         -> String -- ^ prefix to append to the identifiers in the layout, this allows, to load the same dailog/gui element multiple times with different names, if needed
                         -> IO (GUIElement a) -- ^ the resulting tree of GUI elements as top GUI element
loadGuiLayoutFromFile guis layoutFile prefix = do
	let hg3dWinMgr = guiWindowManagerHG3D guis
	window <- WindowManagerHG3D.loadWindowLayoutHG3D hg3dWinMgr layoutFile prefix
	return (GUIElement window undefined)

-- | add a GUI element to the display
addGuiElToDisplay :: GUISystem 
                     -> GUIElement a -- ^ GUI element
                     -> IO ()
addGuiElToDisplay guis (GUIElement window _) = do
	let guir = guiRoot guis
	guiSheet <- System.getGUISheet guir
	Window.addChildWindow2 guiSheet window

-- | remove a GUI element from the display
removeGuiElFromDisplay :: GUISystem 
                          -> GUIElement a -- ^ GUI element
                          -> IO ()
removeGuiElFromDisplay guis (GUIElement window _) = do
	let guir = guiRoot guis
	guiSheet <- System.getGUISheet guir
	Window.removeChildWindow2 guiSheet window

-- | enable GUI element, allow interaction with it and render it accordingly
enableGuiEl :: GUIElement a -- ^ GUI element to enable
               -> IO ()
enableGuiEl (GUIElement window _) = do
	Window.enable window

-- | disable GUI element, disallow interaction with it and render it accordingly
disableGuiEl :: GUIElement a -- ^ GUI element to disable
                -> IO ()
disableGuiEl (GUIElement window _) = do
	Window.disable window

-- | activate GUI element, process events send to it
activateGuiEl :: GUIElement a -- ^ GUI element to activate
                 -> IO ()
activateGuiEl (GUIElement window _) = do
	Window.activate window

-- | deactivate GUI element, do not process events send to it
deactivateGuiEl :: GUIElement a -- ^ GUI element to deactivate
                   -> IO ()
deactivateGuiEl (GUIElement window _) = do
	Window.deactivate window

-- | show GUI element
showGuiEl :: GUIElement a -- ^ GUI element to show
             -> IO ()
showGuiEl (GUIElement window _) = do
	Window.show window

-- | hide GUI element
hideGuiEl :: GUIElement a -- ^ GUI element to hide
             -> IO ()
hideGuiEl (GUIElement window _) = do
	Window.hide window

-- | get child gui element, throws an exception, if element not found in children, only searches the direct children of element.
getChildGuiEl :: GUIElement a -- ^ GUI element, which childrens are searched for child element
                 -> String    -- ^ name of searched child element
                 -> IO (GUIElement a) -- ^ found element (throws exception, in case no child found)
getChildGuiEl (GUIElement window _) name = do
	window <- Window.getChild window name
	return (GUIElement window undefined)

-- | find child element recursively, searches all sub-trees
findChildGuiElRecursive :: GUIElement a -- ^ GUI element, which childrens are searched, including children of children and so on.
                           -> String -- ^ name of child element to be found
                           -> IO (Maybe (GUIElement b)) -- ^ in case of found element: Just el, Nothing otherwhise
findChildGuiElRecursive (GUIElement window _) name = do
	window <- Window.getChildRecursive window name
	if (ocPtr window) == nullPtr then do
		return Nothing
		else do
			return (Just (GUIElement window undefined))

-- | get a named property as string from element
getGuiElProperty :: GUIElement a -- ^ GUI element
                    -> String -- ^ property name to get
                    -> IO String -- ^ property value
getGuiElProperty (GUIElement window _) name = do
	prop <- PropertySet.getProperty window name
	return prop

-- | set a named property 
setGuiElProperty :: GUIElement a -- ^ GUI element
                    -> String  -- ^ property name to set
                    -> String -- ^ property value as string
                    -> IO ()
setGuiElProperty (GUIElement window _) name value = do
	PropertySet.setProperty window name value

-- | load a complete GUI scheme, see CEGUI manual for details
loadGuiScheme :: GUISystem -> String -- ^ scheme name
                 -> IO ()
loadGuiScheme guis schemeName = do
	SystemHG3D.schemeManagerCreate (guiSchemeManager guis) schemeName

-- | load a GUI font
loadGuiFont :: GUISystem -> String -- ^ font name
               -> IO ()	
loadGuiFont guis fontName = do
	SystemHG3D.fontManagerCreate (guiFontManager guis) fontName
	
-- | set default GUI font
setGuiDefaultFont :: GUISystem -> String -- ^ font name
                     -> IO ()	
setGuiDefaultFont guis fontName = do
	let guir = guiRoot guis
	System.setDefaultFont guir fontName
	
-- | set default mouse cursor
setGuiDefaultMouseCursor :: GUISystem 
                            -> String -- ^ scheme name, where cursor is contained in
                            -> String -- ^ cursor name
                            -> IO ()	
setGuiDefaultMouseCursor guis schemeName cursorName = do
	let guir = guiRoot guis
	System.setDefaultMouseCursor3 guir schemeName cursorName

-- | set default tool tip
setGuiDefaultTooltip :: GUISystem -> String -- ^ default tool tip name
                        -> IO ()	
setGuiDefaultTooltip guis ttName = do
	let guir = guiRoot guis
	System.setDefaultTooltip2 guir ttName




-- | initialize the GUI system, used internally
initGUI :: Bool  -- ^ logging flag
           -> Bool -- ^ show graphics cursor flag
           -> IO (GUISystem) -- ^ GUI system object
initGUI fShowGraphicsCursor fLog = do


    -- CEGUI system
    ---------------
    
    -- first logger, loglevel
    
    if not fLog then do
    ceguilog <- CEGUISystemHG3D.createNoLogger 
    return ()
    else do
        ceguilog <- CEGUIDefaultLogger.new
        CEGUILogger.setLogFilename ceguilog "hgamer3d-gui.log" False
        CEGUILogger.setLoggingLevel  ceguilog LoggingLevelInformative
        return ()

    -- bootstrap complete cegui system
    
    guirenderer <- CEGUIOgreRenderer.bootstrapSystem
    
    guiroot <- CEGUISystem.getSingletonPtr
    guiwindowmgr <- CEGUIWindowManagerHG3D.getSingleton
    guifontmgr <- CEGUISystemHG3D.getFontManagerSingleton
    guischememgr <- CEGUISystemHG3D.getSchemeManagerSingleton
    
    CEGUIScheme.setDefaultResourceGroup "Schemes"
    CEGUIImageset.setDefaultResourceGroup "Imagesets"
    CEGUIFont.setDefaultResourceGroup "Fonts"
    CEGUIWidgetLookManager.setDefaultResourceGroup "LookNFeel"
    CEGUIWindowManager.setDefaultResourceGroup "Layouts"
--    CEGUIScriptModule.setDefaultResourceGroup "lua_scripts"

    CEGUISystemHG3D.schemeManagerCreate guischememgr "VanillaSkin.scheme"
    CEGUISystemHG3D.schemeManagerCreate guischememgr "WindowsLook.scheme"
    CEGUISystemHG3D.schemeManagerCreate guischememgr "WindowsLookWidgets.scheme"
    CEGUISystemHG3D.schemeManagerCreate guischememgr "TaharezLook.scheme"
    CEGUISystemHG3D.schemeManagerCreate guischememgr "TaharezLookWidgets.scheme"
        
    CEGUISystemHG3D.fontManagerCreate guifontmgr "DejaVuSans-10.font"
    
    CEGUISystem.setDefaultFont guiroot "DejaVuSans-10"
    if fShowGraphicsCursor then do
      CEGUISystem.setDefaultMouseCursor3 guiroot "Vanilla-Images" "MouseArrow"
      showCursor False
      return ()
      else do
        showCursor True
        return ()
    CEGUISystem.setDefaultTooltip2 guiroot "WindowsLook/Tooltip"
    
    -- set standard empty gui sheet
    myRoot <- CEGUIWindowManager.createWindow guiwindowmgr "DefaultWindow" "root"
    CEGUISystem.setGUISheet guiroot myRoot
        
    eventController <- HG3DEventController.new
    guiwmgrhg3d  <- HG3DWindowManager.new
    uniqueName <- createUniqueName "GUI"
    
    return (GUISystem guiroot guirenderer guiwindowmgr guiwmgrhg3d guifontmgr guischememgr eventController uniqueName)

freeGUI :: GUISystem -> IO ()
freeGUI gs = return ()

keyscanToScancode :: EnumSDLScancode -> Maybe Int
keyscanToScancode keyscan = if out == KeyMediaSelect then Nothing else (Just (fromIntegral (fromEnum out))) where
  out = case keyscan of
    SDL_SCANCODE_RETURN -> KeyReturn
    SDL_SCANCODE_ESCAPE -> KeyEscape
    SDL_SCANCODE_BACKSPACE -> KeyBackspace
    SDL_SCANCODE_TAB -> KeyTab
    SDL_SCANCODE_INSERT -> KeyInsert
    SDL_SCANCODE_HOME -> KeyHome
    SDL_SCANCODE_PAGEUP -> KeyPageUp
    SDL_SCANCODE_DELETE -> KeyDelete
    SDL_SCANCODE_END -> KeyEnd 
    SDL_SCANCODE_PAGEDOWN -> KeyPageDown
    SDL_SCANCODE_RIGHT -> KeyArrowRight
    SDL_SCANCODE_LEFT -> KeyArrowLeft
    SDL_SCANCODE_DOWN -> KeyArrowDown
    SDL_SCANCODE_UP ->KeyArrowUp
    _ -> KeyMediaSelect   -- misused as Nothing indicator
    
    
_injectKeyUp guiroot keycode = do  
  case keyscanToScancode keycode of
    Nothing -> return False
    Just scancode -> System.injectKeyUp guiroot scancode

_injectKeyDown guiroot keycode = do  
  case keyscanToScancode keycode of
    Nothing -> return False
    Just scancode -> System.injectKeyDown guiroot scancode

-- keypressInject

-- | injects key presses into the CEGUI engine, used internally
injectWinEventToGUI :: GUISystem -> SDLEvent -> IO ()
injectWinEventToGUI  guis evt = do
  
  let guiroot = guiRoot guis
  case evt of
    EvtText ts window text -> System.injectChar guiroot (fromEnum (text !! 0))
    EvtKeyUp ts window keyscan keycode keymode -> _injectKeyUp guiroot keyscan
    EvtKeyDown ts window keyscan keycode keymode -> _injectKeyDown guiroot keyscan
    EvtMouseButtonUp ts window mid button x y -> do
                                                 case button of
                                                      SDLButtonLeft -> System.injectMouseButtonUp guiroot CEGUIButton.MouseLeftButton
                                                      SDLButtonRight -> System.injectMouseButtonUp guiroot CEGUIButton.MouseRightButton
                                                      SDLButtonMiddle -> System.injectMouseButtonUp guiroot CEGUIButton.MouseMiddleButton
                                                      _ -> return True
    EvtMouseButtonDown ts window mid button x y -> do
                                                 case button of
                                                      SDLButtonLeft -> System.injectMouseButtonDown guiroot CEGUIButton.MouseLeftButton
                                                      SDLButtonRight -> System.injectMouseButtonDown guiroot CEGUIButton.MouseRightButton
                                                      SDLButtonMiddle -> System.injectMouseButtonDown guiroot CEGUIButton.MouseMiddleButton
                                                      _ -> return True
    EvtMouseMotion ts window mid x y rx ry -> CEGUISystem.injectMousePosition guiroot (fromIntegral x) (fromIntegral y)
    _ -> return True
    
  return ()    
    
-- | register a GUI Event, for later retrieval
registerGUIEvent :: GUISystem 
                    -> GUIElement a  -- ^ GUI Element from which event is registered
                    -> String        -- ^ event name, for which registration is done
                    -> String        -- ^ identifier, with which event is identified later on
                    -> IO ()
registerGUIEvent guis (GUIElement el _) eventToRegister registrationTag = do
  EvtSF.subscribeScriptedEvent el eventToRegister registrationTag
  
data GUIEvent = GUIEvent String String (GUIElement ())

pollGUIEvents :: GUISystem -> IO [GUIEvent]
pollGUIEvents guis = do
        let eventController = guiEventController guis
        processEvents <- HG3DEventController.eventsAvailable eventController
        if processEvents then do
                (name, sender, window) <- HG3DEventController.popEvent eventController
                let evt = GUIEvent name sender (GUIElement window ())
                moreEvents <- pollGUIEvents guis
                return $ (evt : moreEvents)
                else do
                        return []

