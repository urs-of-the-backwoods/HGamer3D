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

guiEventW :: WireSystem -> String -> String -> IO (GameWire a (Event a))
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

buttonW :: WireSystem
           -> String -- ^ GUI element (which should be a button)
           -> IO (GameWire a (Event a)) -- ^ event wire
buttonW ws widgetname = do
  wire <- guiEventW ws widgetname "Clicked" 
  return wire

guiPropertyW :: WireSystem
                -> String
                -> String  -- ^ property name
                -> IO (GameWire (Event a, Event String) (Event String))
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

_createGUIValueW :: WireSystem -> String -> String -> String -> IO (GameWire (Event a, Event String) (Event String))
_createGUIValueW ws widgetname propname eventchangename = do
  widget <- getGuiWidget ws widgetname
  propW <- guiPropertyW ws widgetname propname
  eventW <- guiEventW ws widgetname eventchangename
  let wire = proc (evtL, evtR) -> do
        changeEvent <- eventW -< ()
        let evtLN = fmap (\x -> ()) evtL
        inhibitFlag <- pure False . holdFor 0.5 <|> pure True -< evtR
        evtL' <- arr (\(l, c, p) -> mergeL l (if p then c else NoEvent) ) -< (evtLN, changeEvent, inhibitFlag)
        rVal <- propW -< (evtL', evtR)
        returnA -< rVal
  return wire
    
-- | Constructor for two wires, which in combination provide the functionality of an editable box GUI element.
editBoxW :: WireSystem 
            -> String -- ^ GUI element (should be an editbox)
            -> IO (GameWire (Event a, Event String) (Event String) )
editBoxW ws widgetname = _createGUIValueW ws widgetname "Text" "TextChanged" 

_strtd :: Double -> GameWire (Event String) (Event Double)
_strtd def = (arr . fmap) (\str -> case (reads str)::[(Double, String)] of
                          [(a, str)] -> a
                          _ -> def)
_dtstr :: GameWire (Event Double) (Event String)
_dtstr = (arr . fmap) (\d -> show d)

doubleBoxW :: WireSystem 
            -> String -- ^ GUI element (should be an editbox)
            -> Double -- ^ Default double in case instring does not produce a value
            -> IO (GameWire (Event a, Event Double) (Event Double) )
doubleBoxW ws widgetname def = do
  wire <- editBoxW ws widgetname
  let wire' = _strtd def . wire . (second _dtstr)
  return wire'
	
-- | Constructur for a wire, which can change a static text GUI element (a setter).
staticTextW :: WireSystem 
               -> String -- ^ GUI element (should be a static text, or any widget with a "Text" property.
               -> IO (GameWire (Event a, Event String) (Event String)) -- ^ the returned wire
staticTextW ws widgetname = guiPropertyW ws widgetname "Text"

_bst = arr (\val -> fmap (\b -> if b then "True" else "False") val)
_stb = arr (\val -> fmap (\st -> if st == "True" then True else False) val)

-- | Consructor for two wires, which deliver the checkbox GUI element functionality.
checkBoxW :: WireSystem
             -> String -- ^ GUI element (should be a checkbox)
             -> IO (GameWire (Event a, Event Bool) (Event Bool) )
checkBoxW ws widgetname = do
  wire <- _createGUIValueW ws widgetname "Selected" "CheckStateChanged" 
  return $ _stb . wire . second _bst
  
-- | Constructor for two wires, which deliver the radiobuttion GUI element functionality.
radioButtonW :: WireSystem
                -> String -- ^ GUI element (should be a checkbox)
                -> IO (GameWire (Event a, Event Bool) (Event Bool) )
radioButtonW  ws widgetname = do
  wire <- _createGUIValueW ws widgetname "Selected" "SelectStateChanged"
  return $ _stb . wire . second _bst
  
-- | Constructor for two wires, which deliver the slider GUI element functionality.
sliderW :: WireSystem
           -> String -- ^ GUI element (should be a slider)
           -> Double -- ^ Default Double
           -> IO (GameWire (Event a, Event Double) (Event Double)) -- ^ the returned wires, a value changed wire and a setter wire
sliderW ws widgetname def = do
  wire <- _createGUIValueW ws widgetname "CurrentValue" "ValueChanged"
  return $ _strtd def . wire . second _dtstr

-- | Constructur for two wires, which deliver the spinner GUI element functionality.
spinnerW :: WireSystem
           -> String -- ^ GUI element (should be a spinner)
           -> Double -- ^ Default Double
           -> IO (GameWire (Event a, Event Double) (Event Double)) -- ^ the returned wires, a value changed wire and a setter wire
spinnerW = sliderW




-- LISTBOX AND COMBOBOX
-----------------------

_guiPropertyListboxW :: GUIElement
                        -> IO (GameWire (Event a, Event [(String, Bool, b)]) (Event [(String, Bool, b)]))
_guiPropertyListboxW widget = do
  let wire slist = mkGenN (\(evtL, evtR) -> do
                                             let rVal slist' = do
                                                   case evtL of
                                                     Event _ -> do
                                                       val <- listboxStatus widget
                                                       let val' = fmap (\( (e', s'), (e, s, n)) -> (e', s', n)) (zip val slist')
                                                       return (Right (Event val'), wire slist')
                                                     _ -> return (Right NoEvent, wire slist')
                                                   
                                             case evtR of
                                               Event value -> do
                                                 let value' = fmap (\(e, s, n) -> (e, s)) value
                                                 listboxInitialize widget value'
                                                 r <- rVal value
                                                 return r
                                               _ -> do
                                                 r <- rVal slist
                                                 return r)
  return $ wire []

listBoxW :: WireSystem
            -> String -- ^ GUI element
            -> IO (GameWire (Event a, Event [(String, Bool, b)]) (Event [(String, Bool, b)]))  -- ^ In: request val, set list, set choices
listBoxW ws widgetname = do
  widget <- getGuiWidget ws widgetname
  propW <- _guiPropertyListboxW widget
  eventW <- guiEventW ws widgetname "ItemSelectionChanged"
  let wire = proc (evtL, evtR) -> do
        changeEvent <- eventW -< ()
        let evtLN = fmap (\x -> ()) evtL
        inhibitFlag <- pure False . holdFor 0.5 <|> pure True -< evtR
        evtL' <- arr (\(l, c, p) -> mergeL l (if p then c else NoEvent) ) -< (evtLN, changeEvent, inhibitFlag)
        rVal <- propW -< (evtL', evtR)
        returnA -< rVal
  return wire
    


