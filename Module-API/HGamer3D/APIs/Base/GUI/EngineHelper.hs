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

-- EngineHelper.hs

-- |Supporting GUI functionality for the Base API, used internally for the game loop.
module HGamer3D.APIs.Base.GUI.EngineHelper

(
		initGUIEngine,
		keypressInject
)

where

import GHC.Ptr

import HGamer3D.Data.HG3DClass
import HGamer3D.Data.Vector

import HGamer3D.Bindings.CEGUI.ClassPtr
import HGamer3D.Bindings.CEGUI.Utils

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
import HGamer3D.Bindings.Ogre.ClassHG3DMessagePump as MessagePump


import HGamer3D.APIs.Base.Engine.Types

import Control.Monad.Trans
import Control.Monad.Reader


-- | initialize the GUI system, used internally
initGUIEngine :: Bool  -- ^ logging flag
			-> IO (GUISystem) -- ^ GUI system object
initGUIEngine fLog = do


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
	
	guisystem <- CEGUISystem.getSingletonPtr
	guiwindowmgr <- CEGUIWindowManagerHG3D.getSingleton
	guifontmgr <- CEGUISystemHG3D.getFontManagerSingleton
	guischememgr <- CEGUISystemHG3D.getSchemeManagerSingleton
	
	CEGUIScheme.setDefaultResourceGroup "Schemes"
	CEGUIImageset.setDefaultResourceGroup "Imagesets"
	CEGUIFont.setDefaultResourceGroup "Fonts"
	CEGUIWidgetLookManager.setDefaultResourceGroup "LookNFeel"
	CEGUIWindowManager.setDefaultResourceGroup "Layouts"
--	CEGUIScriptModule.setDefaultResourceGroup "lua_scripts"

	CEGUISystemHG3D.schemeManagerCreate guischememgr "VanillaSkin.scheme"
	CEGUISystemHG3D.schemeManagerCreate guischememgr "WindowsLook.scheme"
	CEGUISystemHG3D.schemeManagerCreate guischememgr "WindowsLookWidgets.scheme"
	CEGUISystemHG3D.schemeManagerCreate guischememgr "TaharezLook.scheme"
	CEGUISystemHG3D.schemeManagerCreate guischememgr "TaharezLookWidgets.scheme"
        
	CEGUISystemHG3D.fontManagerCreate guifontmgr "DejaVuSans-10.font"
	
	CEGUISystem.setDefaultFont guisystem "DejaVuSans-10"
	CEGUISystem.setDefaultMouseCursor3 guisystem "Vanilla-Images" "MouseArrow"
	CEGUISystem.setDefaultTooltip2 guisystem "WindowsLook/Tooltip"
	
	-- set standard empty gui sheet
	myRoot <- CEGUIWindowManager.createWindow guiwindowmgr "DefaultWindow" "root"
	CEGUISystem.setGUISheet guisystem myRoot
		
	eventController <- HG3DEventController.new
	
	guiwmgrhg3d  <- HG3DWindowManager.new
	
	return (GUISystem guirenderer guisystem guiwindowmgr guiwmgrhg3d guifontmgr guischememgr eventController)


-- keypressInject

-- | injects key presses into the CEGUI engine, used internally
keypressInject :: MHGamer3D ()
keypressInject  = do
	rs <- ask
	let g3s = graphics3DSystem rs
	let gui = guiSystem rs
	let gSystem = guiGUI gui
	let messagePump = g3sMessagePump g3s
	
	processEvents <- liftIO $ MessagePump.eventsPending messagePump
	if processEvents then do
		key <- liftIO $ MessagePump.getKeyDownEvent messagePump
		if key /= 0 then do
			liftIO $ CEGUISystem.injectKeyDown gSystem key
			return ()
			else do
				return ()
		key <- liftIO $ MessagePump.getKeyUpEvent messagePump
		if key /= 0 then do
			liftIO $ CEGUISystem.injectKeyUp gSystem key
			return ()
			else do
				return ()
		char <- liftIO $ MessagePump.getCharEvent messagePump
		if char /= 0 then do
			liftIO $ CEGUISystem.injectChar gSystem char
			return ()
			else do
				return ()
                                
                keypressInject                
		return ()
		else do
			return ()
			
