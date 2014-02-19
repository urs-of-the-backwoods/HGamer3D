-- This source file is part of HGamer3D
-- (A project to enable 3D game development in Haskell)
-- For the latest info, see http://www.althainz.de/HGamer3D.html
--
-- (c) 2014 Peter Althainz
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


-- GUI-Widgets.hs

module Main where

import HGamer3D                              -- this import HGamer3D with GUI and Graphics3d and WinEvents
import Control.Monad.Trans
import Data.Maybe

printEvent :: GUIElement -> GUIEvent -> IO ()
printEvent outbox event = do
        case event of
                GUIEvent tag sender window -> do
                        oldtext <- getGuiElProperty outbox "Text"
                        let evttext = tag
                        details <- case sender of
                                "CheckboxWidget" -> do
                                        sel <- getGuiElProperty window "Selected"
                                        return (sel)
                                "CheckboxWidget2" -> do
                                        sel <- getGuiElProperty window "Selected"
                                        return (sel)
                                "EditboxWidget" -> do
                                        sel <- getGuiElProperty window "Text"
                                        return ("\n " ++ sel)
                                "RadiobuttonWidget" -> do
                                        sel <- getGuiElProperty window "Selected"
                                        return (sel)
                                "RadiobuttonWidget2" -> do
                                        sel <- getGuiElProperty window "Selected"
                                        return (sel)
                                "SpinnerWidget" -> do
                                        sel <- getGuiElProperty window "CurrentValue"
                                        return (sel)
                                "SliderWidget" -> do
                                        sel <- getGuiElProperty window "CurrentValue"
                                        return (sel)
                                "ListboxWidget" -> do
                                        sel <- listboxGetSelectedText window
                                        let sel' = if length sel>0 then foldl (\s1 s2 -> (s1 ++ "\n " ++ s2)) [] sel else ""
                                        return (sel')
                                "ComboboxWidget" -> do
                                        sel <- getGuiElProperty window "Text"
                                        return (sel)
                                "ButtonWidget" -> do
                                        return "clicked"
                                _ -> return "no details"
                                        
                        let oldtext' = if length oldtext > 250 then drop 50 oldtext else oldtext
                        setGuiElProperty outbox "Text" (oldtext' ++ "\n" ++ evttext ++ ": " ++ details)
                        return ()
                _ -> do return ()


checkEvents outtext g3ds guis = do
  (evt, qFlag) <- loopHGamer3D g3ds guis
  if qFlag then
    return False
    else
    case evt of
      Just (EventGUI evts) -> do    
        mapM (\evt -> printEvent outtext evt) evts
        return True
      Just (EventWindow (EvtQuit ts)) -> return False
      _ -> return True
      
renderLoop cube outtext g3ds guis = do
   -- rotate 
  yaw3D cube (Rad 0.005) 
  roll3D cube (Rad 0.002)
  proceed <- checkEvents outtext g3ds guis
  if proceed then renderLoop cube outtext g3ds guis else return ()
   
                                                         
registerWidgetEvent guis rootWindow widgetName eventName tagName = do
        widget <- findChildGuiElRecursive rootWindow widgetName
        case widget of
                Just widgetOk -> do
                        registerGUIEvent guis widgetOk eventName tagName
                        return ()
                Nothing -> do
                        return ()                                                         
                                                         
white = Colour 1.0 1.0 1.0 1.0

main = do 
  
        (g3ds, guis, camera, viewport) <- initHGamer3D "HGamer3D - GUI Widgets Example" True False True
  
	-- camera position
	let pos = Vec3 5.0 5.0 80.0
        positionTo3D camera pos
	let at = Vec3 0.0 0.0 (-300.0)
        cameraLookAt camera at
	
	-- define light
            
	setAmbientLight g3ds white
	pointLight g3ds white (Vec3 10.0 10.0 20.0)
        let cube = cubeMesh
        cube <- object3DFromMesh g3ds cube (Just (ResourceMaterial "Colours/Blue") ) False
        
	-- create a shiny blue cube
        positionTo3D cube (Vec3 0.0 0.0 0.0)
        scaleTo3D cube (Vec3 0.2 0.2 0.2)
        
	-- GUI Code starts here, display hg3d logo
	loadGuiScheme guis "hg3d.scheme"
	logo <- loadGuiLayoutFromFile guis "hgamer3d.layout" ""
	addGuiElToDisplay guis logo

	
	-- display gui elements
	guiwidgets <- loadGuiLayoutFromFile guis "gui-widgets.layout" ""
	sttext <- loadGuiLayoutFromFile guis "statictext.layout" ""
	addGuiElToDisplay guis guiwidgets
        
        -- add lists to listbox and combobox
	listbox <- findChildGuiElRecursive guiwidgets "ListboxWidget"
        outtext <- fmap fromJust ( findChildGuiElRecursive guiwidgets "OuttextWidget")
        
	case listbox of
		Just widgetOk -> do
                        setGuiElProperty widgetOk "MultiSelect" "True"
			mapM (listboxAddText widgetOk) ["Entry One", "Entry Two", "Entry Three", "Entry Four", "Entry Five", "Entry Six"]
			return ()
		Nothing -> do
			return ()
                        
	combobox <- findChildGuiElRecursive guiwidgets "ComboboxWidget"
	case combobox of
		Just widgetOk -> do
			mapM (comboboxAddText widgetOk) ["Choice One", "Choice Two", "Choice Three"]
                        setGuiElProperty widgetOk "ReadOnly" "True"
			return ()
		Nothing -> do
			return ()
        
        -- register the events needed
        registerWidgetEvent guis guiwidgets "ButtonWidget" "Clicked" "bw click"
        registerWidgetEvent guis guiwidgets "CheckboxWidget" "CheckStateChanged" "cbw"
        registerWidgetEvent guis guiwidgets "CheckboxWidget2" "CheckStateChanged" "cbw2"
        registerWidgetEvent guis guiwidgets "EditboxWidget" "TextAccepted" "ebw"
        registerWidgetEvent guis guiwidgets "EditboxWidget" "TextChanged" "wbw2"
        registerWidgetEvent guis guiwidgets "RadiobuttonWidget" "SelectStateChanged" "rbw"
        registerWidgetEvent guis guiwidgets "RadiobuttonWidget2" "SelectStateChanged" "rbw2"
        registerWidgetEvent guis guiwidgets "SpinnerWidget" "ValueChanged" "spw"
        registerWidgetEvent guis guiwidgets "SliderWidget" "ValueChanged" "slw"
        registerWidgetEvent guis guiwidgets "ListboxWidget" "ItemSelectionChanged" "lbw"
        registerWidgetEvent guis guiwidgets "ComboboxWidget" "ListSelectionAccepted" "cbw"
                        
	-- start render loop
	renderLoop cube outtext g3ds guis
        exitHGamer3D g3ds guis
        return ()
