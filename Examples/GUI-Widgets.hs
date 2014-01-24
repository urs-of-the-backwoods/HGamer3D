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


-- GUI-Widgets.hs

module Main where

import HGamer3D.APIs.Base
import Control.Monad.Trans


-- renderStep a :: TimeMS -> a -> MEngine3D (Bool, a)
renderStep (TimeMS delta) (cu, outbox) = do
	q <- orientation3D cu
	let r = rotU (Vec3 0.0 0.5 1.0) ((fromIntegral delta)*2*pi/5000.0)
	let q' = r .*. q
	orientationTo3D cu q'
	return (True, (cu, outbox))

printEvent :: GUIElement -> EventFunction
printEvent outbox event = do
	case event of
		GUIEvent name sender window -> do
			oldtext <- getGuiElProperty outbox "Text"
			let evttext = sender
			details <- case sender of
				"CheckboxWidget" -> do
					sel <- getGuiElProperty (GUIElement window) "Selected"
					return (sel)
				"CheckboxWidget2" -> do
					sel <- getGuiElProperty (GUIElement window) "Selected"
					return (sel)
				"EditboxWidget" -> do
					sel <- getGuiElProperty (GUIElement window) "Text"
					return ("\n " ++ sel)
				"RadiobuttonWidget" -> do
					sel <- getGuiElProperty (GUIElement window) "Selected"
					return (sel)
				"RadiobuttonWidget2" -> do
					sel <- getGuiElProperty (GUIElement window) "Selected"
					return (sel)
				"SpinnerWidget" -> do
					sel <- getGuiElProperty (GUIElement window) "CurrentValue"
					return (sel)
				"SliderWidget" -> do
					sel <- getGuiElProperty (GUIElement window) "CurrentValue"
					return (sel)
				"ListboxWidget" -> do
					sel <- listboxGetSelectedText (GUIElement window)
                                        let sel' = if length sel>0 then foldl (\s1 s2 -> (s1 ++ "\n " ++ s2)) [] sel else ""
					return (sel')
				"ComboboxWidget" -> do
					sel <- getGuiElProperty (GUIElement window) "Text"
					return (sel)
				"ButtonWidget" -> do
                                        return "clicked"
				_ -> return "no details"
					
                        let oldtext' = if length oldtext > 250 then drop 50 oldtext else oldtext
			setGuiElProperty outbox "Text" (oldtext' ++ "\n" ++ evttext ++ ": " ++ details)
			return ()
		_ -> do return ()


setupWidgetEvent widgetName eventName evtFunction window = do
	widget <- findChildGuiElRecursive window widgetName
	case widget of
		Just widgetOk -> do
			mapGuiElEventToFunction widgetOk eventName evtFunction
			return ()
		Nothing -> do
			return ()

initAll :: MHGamer3D ()
initAll = do

	-- add media locations
	addResourceLocationMedia "media\\materials" 
	addResourceLocationGUI "media\\gui\\layout" "Layout"
	finalizeResourceLocations

	-- some basic background, to prove this is a 3D Window
	c <- getCamera
	positionTo3D c (Vec3 100.0 100.0 0.0)
	cameraLookAt c (Vec3 (-30.0) (0.0) (70.0))
	l1 <- createPointlight (Colour 1.0 1.0 1.0 1.0) (Vec3 (-100.0) 10.0 10.0)
	l2 <- createPointlight (Colour 1.0 1.0 1.0 1.0) (Vec3 100.0 10.0 10.0)
	let col = Colour 1.0 1.0 1.0 1.0
	cu <- createDodekaeder "Examples/PlatonMaterial" col
	scaleTo3D cu (Vec3 10.0 10.0 10.0)
	setAmbientLight (Colour 1.0 1.0 1.0 1.0)
	positionTo3D cu (Vec3 0.0 0.0 0.0)

	-- GUI Code starts here, display hg3d logo
	loadGuiScheme "hg3d.scheme"
	logo <- loadGuiLayoutFromFile "hgamer3d.layout" ""
	addGuiElToDisplay logo

	
	-- display gui elements
	guiwidgets <- loadGuiLayoutFromFile "gui-widgets.layout" ""
	sttext <- loadGuiLayoutFromFile "statictext.layout" ""
	addGuiElToDisplay guiwidgets
        
        -- add lists to listbox and combobox
	listbox <- findChildGuiElRecursive guiwidgets "ListboxWidget"
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
        
	
	-- add ouput text window
	outtext <- findChildGuiElRecursive guiwidgets "OuttextWidget"
	case outtext of
		Just outbox -> do
			addGuiElToDisplay outbox
			setGuiElProperty outbox "Text" "Event Output Window:"
			
			setupWidgetEvent "ButtonWidget" "Clicked" (printEvent outbox) guiwidgets
			setupWidgetEvent "CheckboxWidget" "CheckStateChanged" (printEvent outbox) guiwidgets
			setupWidgetEvent "CheckboxWidget2" "CheckStateChanged" (printEvent outbox) guiwidgets
			setupWidgetEvent "EditboxWidget" "TextAccepted" (printEvent outbox) guiwidgets
			setupWidgetEvent "EditboxWidget" "TextChanged" (printEvent outbox) guiwidgets
			setupWidgetEvent "RadiobuttonWidget" "SelectStateChanged" (printEvent outbox) guiwidgets 
			setupWidgetEvent "RadiobuttonWidget2" "SelectStateChanged" (printEvent outbox) guiwidgets
			setupWidgetEvent "SpinnerWidget" "ValueChanged" (printEvent outbox) guiwidgets
			setupWidgetEvent "SliderWidget" "ValueChanged" (printEvent outbox) guiwidgets
			setupWidgetEvent "ListboxWidget" "ItemSelectionChanged" (printEvent outbox) guiwidgets
			setupWidgetEvent "ComboboxWidget" "ListSelectionAccepted" (printEvent outbox) guiwidgets
			
			-- render loop
			renderLoop 100 (cu, outbox) renderStep
			return ()
			
		Nothing -> do
			return ()
	


main = do 
	hg <- initHGamer3D "HGamer3D - GUI Widgets Example" 
	(l, hg) <- runMHGamer3D hg initAll 
	return ()
