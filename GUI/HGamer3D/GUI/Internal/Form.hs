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

-- GUI/Internal/Form.hs

-- |Form functionality of the GUI. Implementation module with internal data structures exposed. Public API in HGamer3D.GUI.

module HGamer3D.GUI.Internal.Form
where

import HGamer3D.GUI.Internal.Base
import HGamer3D.GUI.Internal.Widgets
import HGamer3D.GUI.Internal.Properties

import HGamer3D.GUI.Schema.Widget
import HGamer3D.GUI.Schema.Layout
import HGamer3D.GUI.Schema.Form
import HGamer3D.GUI.Schema.GUIDim

import Data.Maybe

          
{- ----------------------------------------------------------------
           Functions for ECS API
   ---------------------------------------------------------------- -}

data GUIEngineDataWidget  = GEDButton GUIButton
                           | GEDRadioButton GUIRadioButton
                           | GEDCheckBox GUICheckBox
                           | GEDComboBox GUIComboBox
                           | GEDListBox GUIListBox
                           | GEDSpinner GUISpinner
                           | GEDSlider GUISlider
                           | GEDEditText GUIEditText
                             
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
_getWidget (GEDSingleElement (GEDButton w)) = _toUndef w
_getWidget (GEDSingleElement (GEDRadioButton w)) = _toUndef w
_getWidget (GEDSingleElement (GEDCheckBox w)) = _toUndef w
_getWidget (GEDSingleElement (GEDComboBox w)) = _toUndef w
_getWidget (GEDSingleElement (GEDListBox w)) = _toUndef w
_getWidget (GEDSingleElement (GEDSpinner w)) = _toUndef w
_getWidget (GEDSingleElement (GEDSlider w)) = _toUndef w
_getWidget (GEDSingleElement (GEDEditText w)) = _toUndef w

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

_createProps :: [WidgetProperty] -> [GUIElement a -> IO ()]
_createProps props = 
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
--        MaxSize w h -> pMaxSize =: (w, h)
--        MinSize w h -> pMinSize =: (w, h)
  in map oneProp props

_createWidget :: GUISystem -> Widget -> String -> IO GUIEngineDataElement
_createWidget guis widget typename = do
        case widget of
          Button name props -> button guis typename (_createProps props) >>= return . GEDSingleElement . GEDButton
          RadioButton name val props -> radioButton guis typename (_createProps props) >>= return . GEDSingleElement . GEDRadioButton
          CheckBox name val props -> checkBox guis typename (_createProps props) >>= return . GEDSingleElement . GEDCheckBox
          ComboBox name val props -> comboBox guis typename (_createProps props) >>= return . GEDSingleElement . GEDComboBox
          ListBox name val props -> listBox guis typename (_createProps props) >>= return . GEDSingleElement . GEDListBox
          Spinner name val props -> spinner guis typename (_createProps props) >>= return . GEDSingleElement . GEDSpinner
          Slider name val props -> slider guis typename (_createProps props) >>= return . GEDSingleElement . GEDSlider
          EditText name val props -> editText guis typename (_createProps props) >>= return . GEDSingleElement . GEDEditText

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

removeForm :: GUISystem -> GUIEngineData -> IO ()
removeForm guis form = do
  return ()

updateForm :: GUISystem -> GUIEngineData -> Form -> IO GUIEngineData
updateForm guis edata form = do
  return edata

