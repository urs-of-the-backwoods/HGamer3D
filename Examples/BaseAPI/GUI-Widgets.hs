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

import HGamer3D.Data
import HGamer3D.Engine.BaseAPI
import HGamer3D.Graphics3D.BaseAPI
import HGamer3D.WinEvent.BaseAPI
import HGamer3D.GUI.BaseAPI
import HGamer3D.Graphics3D.Schema.Camera

import Control.Monad.Trans
import Data.Maybe

printEvent :: GUIElement a -> GUIEvent -> IO ()
printEvent outbox event = do
        case event of
                GUIEvent tag sender window -> do
                  newtext <- case tag of
                                  
                              "reset-click" -> do
                                 return "Reset Button Click"
                              "checkbox-one" -> do
                                 cb <- toCheckBox window
                                 sel <- getP cb pSelected
                                 return ("Checkbox One: " ++ (show sel) ++ "\n")
                              "checkbox-two" -> do
                                 cb <- toCheckBox window
                                 sel <- getP cb pSelected
                                 return ("Checkbox Two: " ++ (show sel) ++ "\n")
                              "editbox-done" -> do
                                 sel <- getP window pText
                                 return ("Edittext (Done): " ++ sel ++ "\n")
                              "editbox-change" -> do
                                 sel <- getP window pText
                                 return ("Edittext (Change): " ++ sel ++ "\n")
                              "radio-one" -> do
                                 rb <- toRadioButton window
                                 sel <- getP rb pSelected
                                 return ("Radiobutton One: " ++ (show sel) ++ "\n")
                              "radio-two" -> do
                                 rb <- toRadioButton window
                                 sel <- getP rb pSelected
                                 return ("Radiobutton Two: " ++ (show sel) ++ "\n")
                              "spinner-value" -> do
                                 sp <- toSpinner window
                                 sel <- getP sp pValue
                                 return ("Spinner: " ++ (show sel) ++ "\n")
                              "slider-value" -> do
                                 sl <- toSlider window
                                 sel <- getP sl pValue
                                 return ("Slider: " ++ (show sel) ++ "\n")
                              "listbox-change" -> do
                                  lb <- toListBox window
                                  sel <- listboxGetSelectedText lb
                                  let sel' = if length sel>0 then foldl (\s1 s2 -> (s1 ++ "\n " ++ s2)) [] sel else ""
                                  return ("Listbox:\n" ++ sel' ++ "\n")
                              "combo-done" -> do
                                  sel <- getP window pText
                                  return ("Combobox: " ++ sel ++ "\n")
                                 
                  
                  oldtext <- getP outbox pText
                  let oldtext' = if length oldtext > 250 then drop 50 oldtext else oldtext
                  setP outbox [ pText =: (oldtext' ++ newtext)]
                  return ()
                _ -> do
                       return ()


checkEvents outtext g3ds guis last = do
  (evts, last', qFlag) <- stepHGamer3D g3ds guis last
  if qFlag then
    return (False, last')
    else do
      qList <- mapM (\evt -> do
             case evt of
               (GUIEvt guiEvt) -> do    
                 printEvent outtext guiEvt
                 return False
               (WindowEvt (EvtQuit ts)) -> return True
               _ -> return False
             ) evts
      return (length (filter id qList) == 0, last')
      
renderLoop cubeF outtext g3ds guis last = do
   -- rotate 
  orientation cubeF >>= \o -> return (yaw o (Rad 0.005)) >>= orientationTo cubeF
  orientation cubeF >>= \o -> return (roll o (Rad 0.002)) >>= orientationTo cubeF
  (proceed, last') <- checkEvents outtext g3ds guis last
  if proceed then renderLoop cubeF outtext g3ds guis last' else return ()
   
main = do 
  
        (g3ds, guis, last) <- initHGamer3D "HGamer3D - GUI Widgets Example" False True True
  
	-- camera position
        camera <- addCamera g3ds (Camera (Frustum 5.0 5000.0 (Deg 60)) (Viewport 0 (Rectangle 0.0 0.0 1.0 1.0) black))
	let pos = Vec3 5.0 5.0 80.0
        positionTo camera pos
	let at = Vec3 0.0 0.0 (-300.0)
        cameraLookAt camera at
	
	-- define light
            
	setAmbientLight g3ds white
	pointLight g3ds white white (Vec3 10.0 10.0 20.0)
        
	-- create a shiny blue cube
        cubeFigure <- cube g3ds 0.2 (ResourceMaterial "Colours/Blue")
        positionTo cubeFigure (Vec3 0.0 0.0 0.0)
        sizeTo cubeFigure (Vec3 0.2 0.2 0.2)
        
	-- GUI Code starts here, display hg3d logo
	loadGuiScheme guis "hg3d.scheme"
	logo <- loadGuiLayoutFromFile guis "hgamer3d.layout" ""
	addGuiElToDisplay guis logo

	
	-- display gui elements
	guiwidgets <- loadGuiLayoutFromFile guis "gui-widgets.layout" ""
	sttext <- loadGuiLayoutFromFile guis "statictext.layout" ""
	addGuiElToDisplay guis guiwidgets


        -- handling of single GUI elements
        ----------------------------------

        -- Outtext, output of events
        outtext <- findEditText "OuttextWidget" guiwidgets

        -- Reset Button
        resetButton <- findButton "ButtonWidget" guiwidgets
        registerGUIEvent guis resetButton "Clicked" "reset-click"

        -- Checkbox One and Two
        checkboxOne <- findCheckBox "CheckboxWidget" guiwidgets
        checkboxTwo <- findCheckBox "CheckboxWidget2" guiwidgets
        registerGUIEvent guis checkboxOne "CheckStateChanged" "checkbox-one"
        registerGUIEvent guis checkboxTwo "CheckStateChanged" "checkbox-two"

        -- Editbox Widgets
        editText <- findEditText "EditboxWidget" guiwidgets
        registerGUIEvent guis editText "TextAccepted" "editbox-done"
        registerGUIEvent guis editText "TextChanged" "editbox-change"

        -- Radiobutton One and Two
        radioOne <- findRadioButton "RadiobuttonWidget" guiwidgets
        radioTwo <- findRadioButton "RadiobuttonWidget2" guiwidgets
        registerGUIEvent guis radioOne "SelectStateChanged" "radio-one"
        registerGUIEvent guis radioTwo "SelectStateChanged" "radio-two"

        -- Spinner and Slider
        spinner <- findSpinner "SpinnerWidget" guiwidgets
        slider <- findSlider "SliderWidget" guiwidgets
        registerGUIEvent guis spinner "ValueChanged" "spinner-value"
        registerGUIEvent guis slider "ValueChanged" "slider-value"

        -- Listbox, Entry one, two, three, ...
	listbox <- findListBox "ListboxWidget" guiwidgets
        setGuiElProperty listbox "MultiSelect" "True"
        mapM (listboxAddText listbox) ["Entry One", "Entry Two", "Entry Three", "Entry Four", "Entry Five", "Entry Six"]
        registerGUIEvent guis listbox "ItemSelectionChanged" "listbox-change"

        -- Combobox, Choice one, two, three                        
	combobox <- findComboBox "ComboboxWidget" guiwidgets
	mapM (comboboxAddText combobox) ["Choice One", "Choice Two", "Choice Three"]
        setGuiElProperty combobox "ReadOnly" "True"
        registerGUIEvent guis combobox "ListSelectionAccepted" "combo-done"

	-- start render loop
	renderLoop cubeFigure outtext g3ds guis last
        freeHGamer3D g3ds guis
        return ()
