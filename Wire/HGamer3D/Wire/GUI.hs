{-# LANGUAGE Arrows #-}

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

-- Wire/GUI.hs

-- | The GUI functionality of the FRP API
module HGamer3D.Wire.GUI
(
        -- * fundamental and simple Wires
        guiEventW,
        guiPropertyW,
        guiSetPropertyW,
        guiGetPropertyW,
        
        buttonW,
	staticTextW,
        
        -- * editable GUI element Wires
        -- $EditGUIWire
	editBoxW,
        doubleBoxW,
	checkBoxW,
        radioButtonW,
        
        -- * special elements
        sliderW,
        spinnerW,
        listBoxW
)

where

import HGamer3D
import HGamer3D.GUI

import Control.Monad.Trans

import Control.Monad.Identity (Identity)
import Control.Wire
import Control.Wire.Unsafe.Event
import Prelude hiding ((.), id)

import Data.IORef
import HGamer3D.Wire.Types


-- functions to put gui events into a list and pop them out again, as well as fire, when there is an event in it
--
  
_gatherGUIEvents :: IORef [evtType] -> evtType -> IO ()
_gatherGUIEvents ref event = do
	list <- readIORef ref
	liftIO $ writeIORef ref (list ++ [event])
	return ()

_popGUIEvent :: IORef [evtType] -> IO (Event evtType)
_popGUIEvent ref = do 
 	list <- readIORef ref
	case list of
		[] -> return NoEvent
		(x : xs) -> do
			writeIORef ref xs
			return $ Event x

-- | A wire, which which delivers the input value as an event, when GUI event occurs
guiEventW :: WireSystem -- ^ the Wiresystem
             -> String -- ^ name of widget
             -> String -- ^ name of event
             -> IO (GameWire a (Event a))  -- ^ resulting wire
guiEventW ws widgetname eventname = do
  let guis = wsGui ws
  let un = wsUniqueName ws
  widget <- getGuiWidget ws widgetname
  tagname <- nextUniqueName un
  ref <- liftIO $ newIORef []
  registerGUIEvent guis widget eventname tagname
  registerGUIEventFunction ws tagname (_gatherGUIEvents ref)
  let wire = mkGen_ (\aval -> do
                        rval <- _popGUIEvent ref
                        case rval of
                          Event _ -> return (Right (Event aval))
                          NoEvent -> return (Right NoEvent) )
  return wire

-- | A wire, which delivers input value as event on button click
buttonW :: WireSystem -- ^ the Wiresystem
           -> String -- ^ name of widget
           -> IO (GameWire a (Event a)) -- ^ resulting wire
buttonW ws widgetname = do
  wire <- guiEventW ws widgetname "Clicked" 
  return wire

-- | A property wire, you can set a property and query a property with this wire
guiPropertyW :: WireSystem -- ^ the Wiresystem
                -> String -- ^ name of widget
                -> String  -- ^ name of property
                -> IO (GameWire (Event a, Event String) (Event String)) -- ^ resulting wire, input of left event queries property, right event sets the property
guiPropertyW ws widgetname propname = do
  widget <- getGuiWidget ws widgetname
  let wire = mkGen_ (\(evtL, evtR) -> do
                                             let rVal = do
                                                   case evtL of
                                                     Event _ -> do
                                                       val <- getGuiElProperty widget propname
                                                       return (Right (Event val))
                                                     _ -> return $ Right NoEvent
                                                   
                                             case evtR of
                                               Event value -> do
                                                 setGuiElProperty widget propname value
                                                 r <- rVal
                                                 return r
                                               _ -> do
                                                 r <- rVal
                                                 return r)
  return wire
  
-- | A wire, which sets a property upon event
guiSetPropertyW :: WireSystem -- ^ the Wiresystem
                -> String -- ^ name of widget
                -> String  -- ^ name of property
                -> IO (GameWire (Event String) (Event String)) -- ^ resulting wire, input sets property and is routed through unchanged
guiSetPropertyW ws widgetname propname = do
  wire <- guiPropertyW ws widgetname propname
  let wire' = proc inval -> do
        wire . (never &&& id) -< inval
        returnA -< inval
  return wire'
  
-- | A wire, which queries a property upon event
guiGetPropertyW :: WireSystem -- ^ the Wiresystem
                -> String -- ^ name of widget
                -> String  -- ^ name of property
                -> IO (GameWire (Event a) (Event String)) -- ^ resulting wire, gets current value of property upon in-event
guiGetPropertyW ws widgetname propname = do
  wire <- guiPropertyW ws widgetname propname
  let wire' = proc inval -> do
        outval <- wire . (id &&& never) -< inval
        returnA -< outval
  return wire'
  
_createGUIValueW :: WireSystem -> String -> String -> String -> String -> IO (GameWire (Event String) (Event String, String))
_createGUIValueW ws widgetname propname eventchangename startv = do
  propW <- guiPropertyW ws widgetname propname
  eventW <- guiEventW ws widgetname eventchangename
  let wire = proc setE -> do
        changeE <- eventW -< ()
        setE' <- id &> now . pure startv -< setE
        changeE' <- propW -< (changeE, setE')
        value <- hold -< setE' `mergeL` changeE'
        inhibitFlag <- (pure False . holdFor 0.5) <|> (pure True) -< setE
        changeE'' <- arr (\(c, p) -> if p then c else NoEvent) -< (changeE', inhibitFlag)
        returnA -< (changeE'', value)
  return wire
    
-- | An edit box wire, editable text element
editBoxW :: WireSystem -- ^ the Wiresystem
            -> String -- ^ name of widget
            -> String -- ^ start value
            -> IO (GameWire (Event String) (Event String, String) ) -- ^ resulting wire
editBoxW ws widgetname startv = _createGUIValueW ws widgetname "Text" "TextChanged" startv

                                                     
_strtd :: Double -> GameWire (Event String, String) (Event Double, Double)
_strtd def = let
  convert = (\str -> case (reads str)::[(Double, String)] of
                          [(a, _)] -> a
                          _ -> def)
  in arr (\(evt, val) ->  (fmap convert evt, convert val))
_dtstr :: GameWire (Event Double) (Event String)
_dtstr = (arr . fmap) (\d -> show d)

-- | A wire, which provides an editable double value
doubleBoxW :: WireSystem -- ^ the Wiresystem
            -> String -- ^ name of widget
            -> Double -- ^ start and default value, in case string does not translate to a double value
            -> IO (GameWire (Event Double) (Event Double, Double) ) -- ^ resulting wire
doubleBoxW ws widgetname def = do
  wire <- editBoxW ws widgetname (show def)
  let wire' = _strtd def . wire . _dtstr
  return wire'
	
-- | A wire, which simply displays a text
staticTextW :: WireSystem -- ^ the Wiresystem
               -> String -- ^ name of widget
               -> IO (GameWire (Event String) (Event String)) -- ^ resulting wire
staticTextW ws widgetname = guiSetPropertyW ws widgetname "Text"

_bst = arr (\val -> fmap (\b -> if b then "True" else "False") val)
_stb = let convert = (\st -> if st == "True" then True else False)
       in arr (\(evt, val) -> (fmap convert evt, convert val))

-- | A checkbox wire
checkBoxW :: WireSystem -- ^ the Wiresystem
             -> String -- ^ name of widget
             -> Bool -- ^ start value
             -> IO (GameWire (Event Bool) (Event Bool, Bool) ) -- ^ resulting wire
checkBoxW ws widgetname startv = do
  wire <- _createGUIValueW ws widgetname "Selected" "CheckStateChanged" (show startv)
  return $ _stb . wire . _bst
  
-- | A radiobutton wire
radioButtonW :: WireSystem -- ^ the Wiresystem
                -> String -- ^ name of widget
                -> Bool -- ^ start value
                -> IO (GameWire (Event Bool) (Event Bool, Bool) ) -- ^ resulting wire
radioButtonW  ws widgetname startv  = do
  wire <- _createGUIValueW ws widgetname "Selected" "SelectStateChanged" (show startv)
  return $ _stb . wire . _bst
  
-- | A slider wire
sliderW :: WireSystem -- ^ the Wiresystem
           -> String -- ^ name of widget
           -> Double -- ^ start value
           -> IO (GameWire (Event Double) (Event Double, Double)) -- ^ resulting wire
sliderW ws widgetname def = do
  wire <- _createGUIValueW ws widgetname "CurrentValue" "ValueChanged" (show def)
  return $ _strtd def . wire . _dtstr

-- | A spinner wire (rename of sliderW)
spinnerW :: WireSystem -- ^ the Wiresystem
           -> String -- ^ name of widget
           -> Double -- ^ start value
           -> IO (GameWire (Event Double) (Event Double, Double)) -- ^ resulting wire
spinnerW = sliderW


-- LISTBOX AND COMBOBOX
-----------------------

_guiPropertyListboxW :: GUIElement
                        -> (a -> String)
                        -> IO (GameWire (Event b, Event [(a, Bool)]) (Event [a]) )
_guiPropertyListboxW widget toStringF = do
  let wire slist = mkGenN (\(evtL, evtR) -> do
                                             let rVal slist' = do
                                                   case evtL of
                                                     Event _ -> do
                                                       val <- listboxStatus widget
                                                       let val' = fmap snd (filter (\( (e', s'), entry) -> s') (zip val slist'))  
                                                       return (Right (Event val'), wire slist')
                                                     _ -> return (Right NoEvent, wire slist')
                                                   
                                             case evtR of
                                               Event value -> do
                                                 let value' = fmap (\(aval,s) -> (toStringF aval, s)) value
                                                 let value'' = fmap fst value
                                                 listboxInitialize widget value'
                                                 r <- rVal value''
                                                 return r
                                               _ -> do
                                                 r <- rVal slist
                                                 return r)
  return $ wire []

-- | A listbox wire, , output events are selection changes, output contains selected data
listBoxW :: WireSystem -- ^ the Wiresystem
            -> String -- ^ name of widget
            -> (a -> String) -- ^ function to convert data values to display string
            -> [(a, Bool)] -- ^ start values
            -> IO (GameWire (Event [(a, Bool)]) (Event [a], [a]))  -- ^ resulting wire
listBoxW ws widgetname toStringF startv = do
  widget <- getGuiWidget ws widgetname
  propW <- _guiPropertyListboxW widget toStringF
  eventW <- guiEventW ws widgetname "ItemSelectionChanged"
  let wire = proc setE -> do
        changeE <- eventW -< ()
        setE' <- id &> now . pure startv -< setE
        changeE' <- propW -< (changeE, setE')
        let setE'' = fmap (fmap fst) (fmap (\l -> filter (\(e, s) -> s) l) setE')
        value <- hold -< setE'' `mergeL` changeE'
        inhibitFlag <- (pure False . holdFor 0.5) <|> (pure True) -< setE
        changeE'' <- arr (\(c, p) -> if p then c else NoEvent) -< (changeE', inhibitFlag)
        returnA -< (changeE'', value)
  return wire

