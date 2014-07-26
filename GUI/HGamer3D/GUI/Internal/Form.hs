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
import Control.Monad

          
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
          GEDButton w n -> return oldList
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

