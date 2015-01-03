{-# OPTIONS_HADDOCK hide #-}
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

-- |Base functionality of the GUI. Implementation module with internal data structures exposed. Public API in HGamer3D.GUI.

module HGamer3D.Graphics3D.GUIBase
where

import GHC.Ptr
import Data.List.Split
import Data.Maybe
import Control.Monad

import HGamer3D.Bindings.CEGUI.ClassOgreRenderer as CEGUIOgreRenderer
import HGamer3D.Bindings.CEGUI.ClassSystem as CEGUISystem
import HGamer3D.Bindings.CEGUI.ClassSystemHG3D as CEGUISystemHG3D
import HGamer3D.Bindings.CEGUI.ClassWindowManagerHG3D as CEGUIWindowManagerHG3D
import HGamer3D.Bindings.CEGUI.ClassWindowManager as CEGUIWindowManager
import HGamer3D.Bindings.CEGUI.ClassResourceProvider as CEGUIResourceProvider
import HGamer3D.Bindings.CEGUI.ClassDefaultResourceProvider as CEGUIDefaultResourceProvider
import HGamer3D.Bindings.CEGUI.ClassImageset as CEGUIImageset
import HGamer3D.Bindings.CEGUI.ClassFont as CEGUIFont
import HGamer3D.Bindings.CEGUI.ClassEventSet as EventSet
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


import HGamer3D.Bindings.CEGUI.ClassPtr
import HGamer3D.Bindings.CEGUI.Utils

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
import qualified HGamer3D.Bindings.CEGUI.ClassUDim as Ud

import HGamer3D.Data
import HGamer3D.Common
import HGamer3D.Data.HG3DClass

import HGamer3D.Graphics3D.WinEvent
import HGamer3D.Graphics3D.GUISchema



{- ----------------------------------------------------------------
   Basic Data Types
   ---------------------------------------------------------------- -}

-- | the basic data item, carrying status information on implementation details for GUI system
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

-- type constructors for phantom types

data GEButton = GEButton
data GERadioButton = GERadioButton
data GECheckBox = GECheckBox
data GEEditText = GEEditText
data GEMultilineText = GEMultilineText

data GEListBox = GEListBox
data GEComboBox = GEComboBox

data GESlider = GESlider
data GESpinner = GESpinner

data GEWindow = GEWindow
data GEHLayout = GEHLayout
data GEVLayout = GEVLayout
data GEGridLayout = GEGridLayout

-- | The GUI Element, a window, a button, any widget or widget element, organized in a hierarchy
data GUIElement a = GUIElement HG3DClass a -- ^ only one constructor, phantom type a, to allow GUIElement sub-typing

{- ----------------------------------------------------------------
   Basic GUI Initialization
   ---------------------------------------------------------------- -}

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

-- | delete a GUI element, do not process events send to it
deleteGuiEl :: GUISystem -> GUIElement a -- ^ GUI element to deactivate
                   -> IO ()
deleteGuiEl guis (GUIElement window _) = do
        let wmgr = guiWindowManager guis
        EventSet.removeAllEvents window
	CEGUIWindowManager.destroyWindow wmgr window

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

-- | add a GUI element as a child to another GUI element
addChildGuiEl :: GUIElement a -- ^ parent GUI element
                 -> GUIElement b -- ^ child GUI element
                 -> IO ()
addChildGuiEl (GUIElement parentW _) (GUIElement childW _) = do
	Window.addChildWindow2 parentW childW

-- | remove a GUI element as a child from the parent GUI element
removeChildGuiEl :: GUIElement a -- ^ parent GUI element
                 -> GUIElement b -- ^ child GUI element
                 -> IO ()
removeChildGuiEl (GUIElement parentW _) (GUIElement childW _) = do
	Window.removeChildWindow2 parentW childW

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

instance Show GUIEvent where
  show (GUIEvent str1 str2 _) = "GUI Event - " ++ str1 ++ " " ++ str2

pollGUIEvent :: GUISystem -> IO (Maybe GUIEvent)
pollGUIEvent guis = do
        let eventController = guiEventController guis
        processEvents <- HG3DEventController.eventsAvailable eventController
        if processEvents
          then do
            (name, sender, window) <- HG3DEventController.popEvent eventController
            let evt = GUIEvent name sender (GUIElement window ())
            return $ Just evt
          else return Nothing

injectGUITimeDelta :: GUISystem -> GameTime -> IO ()
injectGUITimeDelta guis gtime = do
  CEGUISystem.injectTimePulse (guiRoot guis) ((fromIntegral . msec) gtime)
  return ()

notifyDisplaySizeChanged :: GUISystem -> Float -> Float -> IO ()
notifyDisplaySizeChanged guis width height = do
  WindowSF.setNewWindowSize (guiRoot guis) width height
  return ()

type GUIElementProperty a b = (GUIElement a -> IO b, GUIElement a -> b -> IO ())

(=:) :: (GUIElementProperty a b) -> b -> (GUIElement a -> IO ())
(=:) prop val = (\val' guiel -> (snd prop) guiel val') val  

setP :: GUIElement a -> [GUIElement a -> IO ()] -> IO [()]
setP guiel ps = sequence $ fmap (\f -> f guiel) ps

getP :: GUIElement a -> GUIElementProperty a b -> IO b
getP guiel p = (fst p) guiel

{- ----------------------------------------------------------------
   GUI Properties
   ---------------------------------------------------------------- -}

type GUIButtonProperty b = GUIElementProperty GEButton b
type GUIEditTextProperty b = GUIElementProperty GEEditText b

type GUIHasSelectionProperty a b = GUIElementProperty a b
type GUIRadioButtonProperty b = GUIHasSelectionProperty GERadioButton b
type GUICheckBoxProperty b = GUIHasSelectionProperty GECheckBox b

type GUIHasValueProperty a b = GUIElementProperty a b
type GUISliderProperty b = GUIHasValueProperty GESlider b
type GUISpinnerProperty b = GUIHasValueProperty GESpinner b

type GUIListBoxProperty b = GUIElementProperty GEListBox b
type GUIComboBoxProperty b = GUIElementProperty GEComboBox b

_stringProp :: String -> GUIElementProperty a String
_stringProp name = ( 
  (flip getGuiElProperty) name, 
  (\pname guiel val -> setGuiElProperty guiel pname val) name
  )

_toBool :: String -> Bool
_toBool instr = case instr of
  "True" -> True
  "False" -> False
  _ -> False

_fromBool :: Bool -> String
_fromBool inb = case inb of
  True -> "True"
  False -> "False"
  _ -> "False"

_toFloat :: String -> Float
_toFloat = read

_fromFloat :: Float -> String
_fromFloat = Prelude.show

_boolProp :: String -> GUIElementProperty a Bool
_boolProp name = ( 
  ((flip getGuiElProperty) name) >=> return . _toBool, 
  ((\pname guiel val -> setGuiElProperty guiel pname (_fromBool val)) name)
  )

_floatProp :: String -> GUIElementProperty a Float
_floatProp name = ( 
  ((flip getGuiElProperty) name) >=> return . _toFloat, 
  ((\pname guiel val -> setGuiElProperty guiel pname (_fromFloat val)) name)
  )

pText :: GUIElementProperty a String
pText = _stringProp "Text"

pDisabled :: GUIElementProperty a Bool
pDisabled = _boolProp "Disabled"

pVisible :: GUIElementProperty a Bool
pVisible = _boolProp "Visible"

pAlpha :: GUIElementProperty a Float
pAlpha = _floatProp "Alpha"

pAlwaysOnTop :: GUIElementProperty a Bool
pAlwaysOnTop = _boolProp "AlwaysOnTop"

pTooltip :: GUIElementProperty a String
pTooltip = _stringProp "Tooltip"

pFont :: GUIElementProperty a String
pFont = _stringProp "Font"

pX :: GUIElementProperty a GUIDim
pX = (getProp, setProp) where
    getProp (GUIElement window _) = do
      ud <- Window.getXPosition window
      scale <- WindowSF.udScale ud
      offset <- WindowSF.udOffset ud
      return $ GUIDim scale offset
    setProp (GUIElement window _) (GUIDim scale offset) = do
      ud <- Ud.new scale offset
      Window.setXPosition window ud
      Ud.delete ud
      return ()

pY :: GUIElementProperty a GUIDim
pY = (getProp, setProp) where
    getProp (GUIElement window _) = do
      ud <- Window.getYPosition window
      scale <- WindowSF.udScale ud
      offset <- WindowSF.udOffset ud
      return $ GUIDim scale offset
    setProp (GUIElement window _) (GUIDim scale offset) = do
      ud <- Ud.new scale offset
      Window.setYPosition window ud
      Ud.delete ud
      return ()

pWidth :: GUIElementProperty a GUIDim
pWidth = (getProp, setProp) where
    getProp (GUIElement window _) = do
      ud <- WindowSF.getWindowWidth window
      scale <- WindowSF.udScale ud
      offset <- WindowSF.udOffset ud
      Ud.delete ud
      return $ GUIDim scale offset
    setProp (GUIElement window _) (GUIDim scale offset) = do
      ud <- Ud.new scale offset
      Window.setWidth window ud
      Ud.delete ud
      return ()

pHeight :: GUIElementProperty a GUIDim
pHeight = (getProp, setProp) where
    getProp (GUIElement window _) = do
      ud <- WindowSF.getWindowHeight window
      scale <- WindowSF.udScale ud
      offset <- WindowSF.udOffset ud
      Ud.delete ud
      return $ GUIDim scale offset
    setProp (GUIElement window _) (GUIDim scale offset) = do
      ud <- Ud.new scale offset
      Window.setHeight window ud
      Ud.delete ud
      return ()

pMargin :: GUIElementProperty a GUIDim
pMargin = (getProp, setProp) where
    getProp (GUIElement window _) = do
      ud <- WindowSF.getWindowMargin window
      scale <- WindowSF.udScale ud
      offset <- WindowSF.udOffset ud
      Ud.delete ud
      return $ GUIDim scale offset
    setProp (GUIElement window _) (GUIDim scale offset) = do
      ud <- Ud.new scale offset
      WindowSF.setWindowMargin window ud
      Ud.delete ud
      return ()

pSelected :: GUIHasSelectionProperty a Bool
pSelected = _boolProp "Selected"

pValue :: GUIHasValueProperty a Float
pValue = _floatProp "CurrentValue"

pTextSelection :: GUIListBoxProperty [(String, Bool)]
pTextSelection = (listboxStatus, listboxInitialize)

pTextChoice :: GUIComboBoxProperty [String]
pTextChoice = (getProp, setProp) where
  setProp w strings = do
    comboboxRemoveAllText w
    mapM (comboboxAddText w) strings
    return ()
  getProp w = do
    comboboxStatus w

{- ----------------------------------------------------------------
   GUI Widgets
   ---------------------------------------------------------------- -}


-- | GUI Element, Sybtype Button
type GUIButton = GUIElement GEButton

-- | GUI Elements, which have a selection
type GUIHasSelection a = GUIElement a

-- | GUI Element, Sybtype CheckBox
type GUICheckBox = GUIHasSelection GECheckBox

-- | GUI Element, Sybtype RadioButton
type GUIRadioButton = GUIHasSelection GERadioButton

-- | GUI Element, Sybtype EditText
type GUIEditText = GUIElement GEEditText

-- | GUI Element, Sybtype MultilineText
type GUIMultilineText = GUIElement GEMultilineText

-- | GUI Element, Sybtype Listbox
type GUIListBox = GUIElement GEListBox

-- | GUI Element, Sybtype Combobox
type GUIComboBox = GUIElement GEComboBox

-- | GUI Element, which have a value
type GUIHasValue a = GUIElement a

-- | GUI Element, Sybtype Slider
type GUISlider = GUIHasValue GESlider

-- | GUI Element, Sybtype Spinner
type GUISpinner = GUIHasValue GESpinner

-- | GUI Element, Subtype FrameWindow
type GUIWindow = GUIElement GEWindow

-- | GUI Element, which is a layout container
type GUILayout a = GUIElement a

-- | GUI Element, vertical layout container
type GUIVLayout = GUILayout GEVLayout

-- | GUI Element, horizontal layout container
type GUIHLayout = GUILayout GEHLayout

-- | GUI Element, grid layout container
type GUIGridLayout = GUILayout GEGridLayout


-- | get Type of GUI element as string
typeOfGuiEl :: GUIElement a -- ^ GUI element to enable
               -> IO String
typeOfGuiEl (GUIElement window _) = do
	Window.getType window

toGuiType :: String -> b -> GUIElement a -> IO (GUIElement b)
toGuiType typestr cons guiel@(GUIElement window _) = do
          tp <- typeOfGuiEl guiel
          -- CEGUI Types as String have the format "WindowLook/Button" for example
          -- sometimes they have no "xyzLook" part and no /
          let tp' = last (splitOn "/" tp)  
          let guiel' = case tp' of
                         typestr -> GUIElement window cons
                         _ -> error ("HGamer3D.GUI.Internal.Widgets.toGuiType: " ++ typestr ++ " not found!")
          return guiel'

_createElement :: String -> (GUIElement a -> IO (GUIElement b)) -> GUISystem -> String -> [GUIElement b -> IO ()] -> IO (GUIElement b)
_createElement elType convFunc guis style proplist = do
  let winMgr = guiWindowManager guis
  let uname = guiUniqueName guis
  elName <- nextUniqueName uname
  window <- WindowManager.createWindow winMgr (style ++ "/" ++ elType) elName
  el <- convFunc (GUIElement window undefined)
  -- set the properties
  setP el proplist 
  return el

_createElement' :: String -> (GUIElement a -> IO (GUIElement b)) -> GUISystem -> [GUIElement b -> IO ()] -> IO (GUIElement b)
_createElement' elType convFunc guis proplist = do
  let winMgr = guiWindowManager guis
  let uname = guiUniqueName guis
  elName <- nextUniqueName uname
  window <- WindowManager.createWindow winMgr (elType) elName
  el <- convFunc (GUIElement window undefined)
  -- set the properties
  setP el proplist 
  return el

button :: GUISystem -> String -> [GUIButton -> IO ()] -> IO GUIButton
button = _createElement "Button" toButton

radioButton :: GUISystem -> String -> [GUIRadioButton -> IO ()] -> IO GUIRadioButton
radioButton = _createElement "RadioButton" toRadioButton

checkBox :: GUISystem -> String -> [GUICheckBox -> IO ()] -> IO GUICheckBox
checkBox = _createElement "Checkbox" toCheckBox

editText :: GUISystem -> String -> [GUIEditText -> IO ()] -> IO GUIEditText
editText = _createElement "Editbox" toEditText

multilineText :: GUISystem -> String -> [GUIMultilineText -> IO ()] -> IO GUIMultilineText
multilineText = _createElement "MultiLineEditbox" toMultilineText

comboBox :: GUISystem -> String -> [GUIComboBox -> IO ()] -> IO GUIComboBox
comboBox = _createElement "Combobox" toComboBox

listBox :: GUISystem -> String -> [GUIListBox -> IO ()] -> IO GUIListBox
listBox = _createElement "Listbox" toListBox

spinner :: GUISystem -> String -> [GUISpinner -> IO ()] -> IO GUISpinner
spinner = _createElement "Spinner" toSpinner

slider :: GUISystem -> String -> [GUISlider -> IO ()] -> IO GUISlider
slider = _createElement "Slider" toSlider

window :: GUISystem -> String -> [GUIWindow -> IO ()] -> IO GUIWindow
window = _createElement "FrameWindow" toWindow

hLayout :: GUISystem -> [GUIHLayout -> IO ()] -> IO GUIHLayout
hLayout = _createElement' "HorizontalLayoutContainer" toHLayout

vLayout :: GUISystem -> [GUIVLayout -> IO ()] -> IO GUIVLayout
vLayout = _createElement' "VerticalLayoutContainer" toVLayout

gridLayout :: GUISystem -> [GUIGridLayout -> IO ()] -> IO GUIGridLayout
gridLayout = _createElement' "GridLayoutContainer" toGridLayout

toButton = toGuiType "Button" GEButton
toRadioButton = toGuiType "RadioButton" GERadioButton
toCheckBox = toGuiType "Checkbox" GECheckBox

toEditText = toGuiType "Editbox" GEEditText
toMultilineText = toGuiType "MultiLineEditbox" GEMultilineText

toComboBox = toGuiType "Combobox" GEComboBox
toListBox = toGuiType "Listbox" GEListBox

toSlider = toGuiType "Slider" GESlider
toSpinner = toGuiType "Spinner" GESpinner

toHLayout = toGuiType "HorizontalLayoutContainer" GEHLayout
toVLayout = toGuiType "VerticalLayoutContainer" GEVLayout
toGridLayout = toGuiType "GridLayoutContainer" GEGridLayout
toWindow = toGuiType "FrameWindow" GEWindow

findElement :: (GUIElement a -> IO (GUIElement b))
               ->String 
               -> GUIElement a 
               -> IO (GUIElement b)
findElement toNewType name topel = do
  mFound <- findChildGuiElRecursive topel name
  el <- toNewType (fromJust mFound)
  return el  

findButton = findElement toButton
findRadioButton = findElement toRadioButton
findCheckBox = findElement toCheckBox
findEditText = findElement toEditText
findMultilineText = findElement toMultilineText
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
	
comboboxStatus :: GUIComboBox -- ^ GUI element, needs to be a listbox
                         -> IO [String] -- ^ list of entry, selected pairs
comboboxStatus (GUIElement window GEComboBox) = do
	realbox <- WindowSF.castWindowToCombobox window
        count <- Combobox.getItemCount realbox
        outlist <- mapM ( \ind -> do
                        item <- Combobox.getListboxItemFromIndex realbox ind
                        txt <- ListboxItem.getText item
                        return txt
                  ) [0..(count-1)]
        return outlist

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
	

          
{- ----------------------------------------------------------------
           Functions for ECS API
   ---------------------------------------------------------------- -}

data GUIEngineDataWidget  = GEDButton GUIButton String
                           | GEDRadioButton GUIRadioButton String
                           | GEDCheckBox GUICheckBox String
                           | GEDComboBox GUIComboBox String
                           | GEDListBox GUIListBox String
                           | GEDSpinner GUISpinner String
                           | GEDSlider GUISlider String
                           | GEDEditText GUIEditText String
                           | GEDMultilineText GUIMultilineText String
                             
data GUIEngineDataLayout = GEDHLayout GUIHLayout
                            | GEDVLayout GUIVLayout
                            | GEDGridLayout GUIGridLayout
                            | GEDWindow GUIWindow

data GUIEngineData = GUIEngineData GUIEngineDataElement Form

data GUIEngineDataElement = GEDSingleElement GUIEngineDataWidget
                            | GEDCombinedElement GUIEngineDataLayout [GUIEngineDataElement]

-- there is a big distinction in the typesystem between a type which is not known to the function but it is assumed it has a specific type on calling (typevariable a) and the return value of being any possible type which is requested by outer world type (typevariable b). So _undef is polymorphic in its out value, this only can be accomplished by undef!
_toUndef :: GUIElement a -> GUIElement b
_toUndef (GUIElement window _) = (GUIElement window undefined)

_getWidget :: GUIEngineDataElement -> GUIElement a
_getWidget (GEDSingleElement (GEDButton w _)) = _toUndef w
_getWidget (GEDSingleElement (GEDRadioButton w _)) = _toUndef w
_getWidget (GEDSingleElement (GEDCheckBox w _)) = _toUndef w
_getWidget (GEDSingleElement (GEDComboBox w _)) = _toUndef w
_getWidget (GEDSingleElement (GEDListBox w _)) = _toUndef w
_getWidget (GEDSingleElement (GEDSpinner w _)) = _toUndef w
_getWidget (GEDSingleElement (GEDSlider w _)) = _toUndef w
_getWidget (GEDSingleElement (GEDEditText w _)) = _toUndef w
_getWidget (GEDSingleElement (GEDMultilineText w _)) = _toUndef w

_getWidget (GEDCombinedElement (GEDHLayout l) _) = _toUndef l
_getWidget (GEDCombinedElement (GEDVLayout l) _) = _toUndef l
_getWidget (GEDCombinedElement (GEDGridLayout l) _) = _toUndef l
_getWidget (GEDCombinedElement (GEDWindow l) _) = _toUndef l

_createLayout :: GUISystem -> Layout -> String -> IO GUIEngineDataLayout
_createLayout guis layout typename = do
        case layout of
          VerticalLayout props -> vLayout guis (_createProps props) >>= return . GEDVLayout
          HorizontalLayout props -> hLayout guis (_createProps props) >>= return . GEDHLayout
          GridLayout x y props -> gridLayout guis (_createProps props) >>= return . GEDGridLayout
          Window name props -> window guis typename (_createProps props) >>= return . GEDWindow

-- the following routine assume same typename for new layout, it only modify props
_updateLayoutProps :: GUIEngineDataLayout -> Layout -> IO ()
_updateLayoutProps edata layout = do
  -- check assumptions
  case (edata, layout) of
    (GEDHLayout hLayout, HorizontalLayout props) -> setP hLayout (_createProps props)
    (GEDVLayout vLayout, VerticalLayout props) -> setP vLayout (_createProps props)
    (GEDGridLayout gLayout, GridLayout x y props) -> setP gLayout (_createProps props)
    (GEDWindow window, Window n props) -> setP window (_createProps props)
    _ -> error "HGamer3D.GUI.Internal.Form._updateLayoutProps: update parameters not matching!"
  return ()
      
_createProps' :: (WidgetProperty -> GUIElement a -> IO ()) -> [WidgetProperty] -> [GUIElement a -> IO ()]
_createProps' addOn props = 
  let oneProp prop = case prop of
        XPos gd -> pX =: gd
        YPos gd -> pY =: gd
        Width gd -> pWidth =: gd
        Height gd -> pHeight =: gd
        Visible v -> pVisible =: v
        Alpha f -> pAlpha =: f
        Text t -> pText =: t
        Margin m -> pMargin =: m
        Tooltip t -> pTooltip =: t
        _ -> addOn prop
  in map oneProp props

_createProps = _createProps' (\w -> (const (return ())))

_createPropsCB :: [WidgetProperty] -> [GUIComboBox -> IO ()]
_createPropsCB props =
  let addOn prop = case prop of
        TextChoice tc -> pTextChoice =: tc
        _ -> const (return ())
  in _createProps' addOn props

_createPropsLB :: [WidgetProperty] -> [GUIListBox -> IO ()]
_createPropsLB props =
  let addOn prop = case prop of
        TextSelection ts -> pTextSelection =: ts
        _ -> const (return ())
  in _createProps' addOn props

_createPropsHV :: [WidgetProperty] -> [GUIHasValue a -> IO ()]
_createPropsHV props =
  let addOn prop = case prop of
        Value v -> pValue =: v
        _ -> const (return ())
  in _createProps' addOn props

_createPropsHS :: [WidgetProperty] -> [GUIHasSelection a -> IO ()]
_createPropsHS props =
  let addOn prop = case prop of
        Selected s -> pSelected =: s
        _ -> const (return ())
  in _createProps' addOn props

_createWidget :: GUISystem -> Widget -> String -> IO GUIEngineDataElement
_createWidget guis widget typename = do
        case widget of
          Button name props -> do
            w <- button guis typename (_createProps props)
            registerGUIEvent guis w "Clicked" name
            return (GEDSingleElement (GEDButton w name))
          RadioButton name props -> do
            w <- radioButton guis typename (_createPropsHS props)
            registerGUIEvent guis w "SelectStateChanged" name
            return (GEDSingleElement (GEDRadioButton w name))
          CheckBox name props -> do
            w <- checkBox guis typename (_createPropsHS props)
            registerGUIEvent guis w "CheckStateChanged" name
            return (GEDSingleElement (GEDCheckBox w name))
          ComboBox name props -> do
            w <- comboBox guis typename (_createPropsCB props)
            registerGUIEvent guis w "ListSelectionAccepted" name
            return (GEDSingleElement (GEDComboBox w name))
          ListBox name props -> do
            w <- listBox guis typename (_createPropsLB props)
            registerGUIEvent guis w "ItemSelectionChanged" name
            return (GEDSingleElement (GEDListBox w name))
          Spinner name props -> do
            w <- spinner guis typename (_createPropsHV props)
            registerGUIEvent guis w "ValueChanged" name
            return (GEDSingleElement (GEDSpinner w name))
          Slider name props -> do
            w <- slider guis typename (_createPropsHV props)
            registerGUIEvent guis w "ValueChanged" name
            return (GEDSingleElement (GEDSlider w name))
          EditText name props -> do
            w <- editText guis typename (_createProps props)
            registerGUIEvent guis w "TextAccepted" name
            registerGUIEvent guis w "TextChanged" name
            return (GEDSingleElement (GEDEditText w name))
          MultilineText name props -> do
            w <- multilineText guis typename (_createProps props)
            registerGUIEvent guis w "TextAccepted" name
            registerGUIEvent guis w "TextChanged" name
            return (GEDSingleElement (GEDMultilineText w name))

_updateWidgetProps :: GUIEngineDataElement -> Widget -> IO ()
_updateWidgetProps edata widget = do
  case (edata, widget) of
    (GEDSingleElement (GEDButton w _), Button name props) -> setP w (_createProps props)
    (GEDSingleElement (GEDRadioButton w _), RadioButton name props) -> setP w (_createPropsHS props)
    (GEDSingleElement (GEDCheckBox w _), CheckBox name props) -> setP w (_createPropsHS props)
    (GEDSingleElement (GEDComboBox w _), ComboBox name props) -> setP w (_createPropsCB props)
    (GEDSingleElement (GEDListBox w _), ListBox name props) -> setP w (_createPropsLB props)
    (GEDSingleElement (GEDSpinner w _), Spinner name props) -> setP w (_createPropsHV props)
    (GEDSingleElement (GEDSlider w _), Slider name props) -> setP w (_createPropsHV props)
    (GEDSingleElement (GEDEditText w _), EditText name props) -> setP w (_createProps props)
    (GEDSingleElement (GEDMultilineText w _), MultilineText name props) -> setP w (_createProps props)
    _ -> error "HGamer3D.GUI.Internal.Form._updateWidgetProps: update parameters not matching!"
  return ()

createForm :: GUISystem -> Form -> IO GUIEngineData
createForm guis form = do
  let (Form typename formcontent) = form
     
  let createFormContent formcontent = do
        case formcontent of
          WidgetFC widget -> _createWidget guis widget typename
          LayoutFC layout formList -> do
            layoutW <- _createLayout guis layout typename
            widgetsW <- mapM createFormContent formList
            let cl = GEDCombinedElement layoutW widgetsW
            mapM (\f -> addChildGuiEl (_getWidget cl) (_getWidget f)) widgetsW
            return $ cl
            
  formW <- createFormContent formcontent
  addGuiElToDisplay guis (_getWidget formW)
  return $ GUIEngineData formW form

getFormValues :: GUIEngineData -> IO [(String, FormValue)]
getFormValues (GUIEngineData elem form) = do

  let addTo oldList n con = return . (flip (:)) oldList . (,) n . con
        
  let getItem oldList eitem = case eitem of
        GEDSingleElement widget -> case widget of
          GEDButton w n -> return ( (n, FVE) : oldList)
          GEDRadioButton w n -> getP w pSelected >>= addTo oldList n FVB
          GEDCheckBox w n -> getP w pSelected >>= addTo oldList n FVB
          GEDComboBox w n -> getP w pText >>= addTo oldList n FVS
          GEDListBox w n -> getP w pTextSelection >>= addTo oldList n FVTS
          GEDSpinner w n -> getP w pValue >>= addTo oldList n FVF
          GEDSlider w n -> getP w pValue >>= addTo oldList n FVF
          GEDEditText w n -> getP w pText >>= addTo oldList n FVS
          GEDMultilineText w n -> getP w pText >>= addTo oldList n FVS

  let foldEitems oldList eitem = case eitem of
        GEDSingleElement widget -> getItem oldList eitem
        GEDCombinedElement _ widgetList -> foldM foldEitems oldList widgetList
          
  foldEitems [] elem

_getFormStructure :: Form -> [String]
_getFormStructure (Form typename content) = 
  
  let getItem oldList eitem = case eitem of
        WidgetFC widget -> case widget of
          Button n _ -> ("Button/" ++ n) : oldList
          RadioButton n _  -> ("RadioButton/" ++ n) : oldList
          CheckBox n _ -> ("CheckBox/" ++ n) : oldList
          ComboBox n _ -> ("ComboBox/" ++ n) : oldList
          ListBox n _ -> ("ListBox/" ++ n) : oldList
          Spinner n _ -> ("Spinner/" ++ n) : oldList
          Slider n _ -> ("Slider/" ++ n) : oldList
          EditText n _ -> ("EditText/" ++ n) : oldList
          MultilineText n _ -> ("MultilineText/" ++ n) : oldList
        LayoutFC layout itemList -> let
          subList = foldl getItem [] itemList
          in case layout of
            VerticalLayout props -> ["VLStart"] ++ subList ++ ["VLEnd"] ++ oldList
            HorizontalLayout props -> ["HLStart"] ++ subList ++ ["HLEnd"] ++ oldList
            GridLayout _ _ props -> ["GridStart"] ++ subList ++ ["GridEnd"] ++ oldList
            Window n props -> ["WStart"] ++ subList ++ ["WEnd"] ++ oldList
            
  in getItem [typename] content


setFormValues :: GUIEngineData ->  [(String, FormValue)] -> IO ()
setFormValues (GUIEngineData elem form) valueList = do

  let filterItemSet w n prop valueList = do
        case filter (\(name, value) -> name == n) valueList of
          [] -> return ()
          ((n', v') : _) -> do
            setP w [prop v']
            return ()
          
  let setItem valueList eitem = case eitem of
        GEDSingleElement widget -> case widget of
          GEDButton w n -> return ()
          GEDRadioButton w n -> filterItemSet w n (\v -> let (FVB v') = v in pSelected =: v') valueList
          GEDCheckBox w n -> filterItemSet w n (\v -> let (FVB v') = v in pSelected =: v') valueList
          GEDComboBox w n -> filterItemSet w n (\v -> let (FVS v') = v in pText =: v') valueList
          GEDListBox w n -> filterItemSet w n (\v -> let (FVTS v') = v in pTextSelection =: v') valueList
          GEDSpinner w n -> filterItemSet w n (\v -> let (FVF v') = v in pValue =: v') valueList
          GEDSlider w n -> filterItemSet w n (\v -> let (FVF v') = v in pValue =: v') valueList
          GEDEditText w n -> filterItemSet w n (\v -> let (FVS v') = v in pText =: v') valueList
          GEDMultilineText w n -> filterItemSet w n (\v -> let (FVS v') = v in pText =: v') valueList
        GEDCombinedElement _ widgetList -> mapM (setItem valueList) widgetList >> return ()

  setItem valueList elem
  return ()

removeForm :: GUISystem -> GUIEngineData -> IO ()
removeForm guis form = do
  let (GUIEngineData elem schema) = form
      
  -- first detach from screen
  removeGuiElFromDisplay guis (_getWidget elem)

  -- then delete the elements in order
  let deleteSingleItem elem = case elem of
        GEDSingleElement w -> deleteGuiEl guis (_getWidget elem)
        GEDCombinedElement layout widgetList -> do
          -- remove childs from parent, then delete
          mapM (\el -> removeChildGuiEl (_getWidget elem) (_getWidget el)) widgetList
          mapM deleteSingleItem widgetList
          deleteGuiEl guis (_getWidget elem)

  deleteSingleItem elem
  return ()


-- update form with same structure, apply properties again for all elements, which are different
_updateFormElement :: GUISystem -> GUIEngineDataElement -> FormContent -> FormContent -> IO ()
_updateFormElement guis elem oldFC newFC = do
  case (elem, oldFC, newFC) of
    (GEDSingleElement w, WidgetFC (Button _ oldProps), WidgetFC wid@(Button _ newProps)) -> do
      if oldProps /= newProps then _updateWidgetProps elem wid else return ()
    (GEDSingleElement w, WidgetFC (RadioButton _ oldProps), WidgetFC wid@(RadioButton _ newProps))  -> do
      if oldProps /= newProps then _updateWidgetProps elem wid else return ()
    (GEDSingleElement w, WidgetFC (CheckBox _ oldProps), WidgetFC wid@(CheckBox _ newProps))  -> do
      if oldProps /= newProps then _updateWidgetProps elem wid else return ()
    (GEDSingleElement w, WidgetFC (ComboBox _ oldProps), WidgetFC wid@(ComboBox _ newProps))  -> do
      if oldProps /= newProps then _updateWidgetProps elem wid else return ()
    (GEDSingleElement w, WidgetFC (ListBox _ oldProps), WidgetFC wid@(ListBox _ newProps))  -> do
      if oldProps /= newProps then _updateWidgetProps elem wid else return ()
    (GEDSingleElement w, WidgetFC (Spinner _ oldProps), WidgetFC wid@(Spinner _ newProps))  -> do
      if oldProps /= newProps then _updateWidgetProps elem wid else return ()
    (GEDSingleElement w, WidgetFC (Slider _ oldProps), WidgetFC wid@(Slider _ newProps))  -> do
      if oldProps /= newProps then _updateWidgetProps elem wid else return ()
    (GEDSingleElement w, WidgetFC (EditText _ oldProps), WidgetFC wid@(EditText _ newProps))  -> do
      if oldProps /= newProps then _updateWidgetProps elem wid else return ()
    (GEDSingleElement w, WidgetFC (MultilineText _ oldProps), WidgetFC wid@(MultilineText _ newProps))  -> do
      if oldProps /= newProps then _updateWidgetProps elem wid else return ()
    (GEDCombinedElement layout elems, LayoutFC oldLayout oldContentList, LayoutFC newLayout newContentList)  -> do
      if oldLayout /= newLayout then _updateLayoutProps layout newLayout else return ()
      mapM (\(elem, oldContent, newContent) -> _updateFormElement guis elem oldContent newContent) (zip3 elems oldContentList newContentList)
      return ()
      


updateForm :: GUISystem -> GUIEngineData -> Form -> IO GUIEngineData
updateForm guis edata form = do
  -- check if different structure, then redo
  let ed@(GUIEngineData elem oldForm) = edata
  if (_getFormStructure oldForm) /= (_getFormStructure form) then do
    removeForm guis ed
    createForm guis form
    -- if structure is the same, only update properties
    else do
      let (Form typename content) = form
      let (Form typename oldContent) = oldForm
      _updateFormElement guis elem oldContent content
      return edata









