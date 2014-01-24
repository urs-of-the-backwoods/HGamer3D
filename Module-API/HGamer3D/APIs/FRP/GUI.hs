{-# LANGUAGE Arrows #-}

-- | The GUI functionality of the FRP API
module HGamer3D.APIs.FRP.GUI
(
        -- * basic Wires
        guiEventW,
        guiSetPropW,
        guiGetPropW,
        buttonW,
	staticTextW,
        
        -- * editable GUI element wires
        -- $EditGUIWire
	editBoxW,
        floatEditBoxW,
	checkBoxW,
	radioButtonW,
	sliderW,
	spinnerW,
        listBoxW,
	comboBoxW,
        
        -- * Wires which do not fire on setting values by program
        -- $FireOnlyOnGUI
	editBoxW',
        floatEditBoxW',
	checkBoxW',
	radioButtonW',
	sliderW',
	spinnerW'
)

where

import HGamer3D.APIs.Base as Base
import HGamer3D.Bindings.CEGUI.ClassHG3DEventController as HG3DEventController

import Control.Monad.Trans

import Control.Monad.Identity (Identity)
import Control.Wire
import Prelude hiding ((.), id)

import Control.Monad.Reader
import Control.Monad.State

import Data.IORef
import HGamer3D.APIs.FRP.Types


-- functions to put gui events into a list and pop them out again, as well as fire, when there is an event in it
--
  
_gatherGUIEvents :: IORef [Base.Event] -> EventFunction
_gatherGUIEvents ref event = do
	list <- liftIO $ readIORef ref
	liftIO $ writeIORef ref (list ++ [event])
	return ()

_popGUIEvent :: IORef [Base.Event] -> MHGamer3D (Maybe Base.Event)
_popGUIEvent ref = do 
	list <- liftIO $ readIORef ref
	case list of
		[] -> return Nothing
		(x : xs) -> do
			liftIO $ writeIORef ref xs
			return $ Just x

_eventFunctionFired :: IORef [Base.Event] -> Time -> a -> MHGamer3D (Either () a)
_eventFunctionFired ref t a = do
	evt <- _popGUIEvent ref
	case evt of
		Just (GUIEvent name sender window) -> do
                  return (Right a)
		Nothing -> do
			return (Left ())
			

-- new approach, more elemental wires, with better granularity
--

-- first create an event receiving wire for a GUIElement and a specific event

-- | Constructor for a wire, which delivers events from a GUI element
guiEventW :: GUIElement -- ^ GUI element
             -> String -- ^ name of event
             -> GameWire a a -- ^ event wire, which fires in case of event occurrence
guiEventW widget eventname = switch (mkStateM True (\t (a,s) -> do
                                                if s then do
                                                  -- create the event IORef
                                                  ref <- liftIO $ newIORef []
                                                  -- then map the events to a function, which gather events into this ref
                                                  mapGuiElEventToFunction widget eventname (_gatherGUIEvents ref)
                                                  -- finally create the wire
                                                  return $ (Right (mkFixM (_eventFunctionFired ref)), False)
                                                  else do
                                                    return (Left (), False))) id
                                                  
                                                  

-- then create a wire which gets and sets a property of the underlying GUI element


-- | Constructor for a wire, which sets a specific property on occurrence
guiSetPropW :: GUIElement -- ^ GUI element
               -> String -- ^ property name
               -> GameWire String String -- ^ wire, which sets the property and returns the value
guiSetPropW widget propname = mkFixM (\t s -> do
                                         setGuiElProperty widget propname s
                                         return $ Right s)

-- | Constructor for a wire, which gets a specific property from a GUI element
guiGetPropW :: GUIElement -- ^ GUI element
               -> String -- ^ property name
               -> GameWire a String -- ^ wire, which returns property value as string, input is ignored
guiGetPropW widget propname = mkFixM (\t s -> do
                                         s' <-getGuiElProperty widget propname
                                         return $ Right s')

_createGUIValueW :: GUIElement -> String -> String -> (GameWire a String, GameWire String String)
_createGUIValueW widget propname changename = (valueW . changeW, setValueW) where
  changeW = guiEventW widget changename
  valueW = guiGetPropW widget propname
  setValueW = guiSetPropW widget propname

-- | Constructor for two wires, which in combination provide the functionality of an editable box GUI element.
editBoxW :: GUIElement -- ^ GUI element (should be an editbox)
            -> (GameWire a String, GameWire String String) -- ^ Two wires as return value, a wire, which fires when the value changed and a setter wire, which sets the text of the editbox.
editBoxW widget =_createGUIValueW widget "Text" "TextChanged" 
	
-- | Constructor for two wires, which provide the functionality of an editable box for float values as a GUI element.
floatEditBoxW :: GUIElement -- ^ GUI element (should be an editbox)
                 -> (GameWire a Float, GameWire Float Float) -- ^ Two wires as return value, a value changed wire and a setter wire.
floatEditBoxW widget = (fchanged, fsetter) where
  sToF str = if length arr > 0 then fst $ arr !! 0 else 0.0 where
    arr = (reads str) :: [(Float, String)]
  toFW = mkFix (\t s -> Right $ sToF s)
  showF = mkFix (\t f -> Right $ show f)
  (changed, setter) = editBoxW widget
  fchanged = toFW . changed
  fsetter = toFW . setter . showF

-- | Constructur for a wire, which can change a static text GUI element (a setter).
staticTextW :: GUIElement -- ^ GUI element (should be a static text, or any widget with a "Text" property.
               -> (GameWire String String) -- ^ the returned wire
staticTextW widget = guiSetPropW widget "Text"

_bst = mkFix (\time b -> if b then Right "True" else Right "False")
_stb = mkFix (\time st -> if st == "True" then Right True else Right False)

-- | Consructor for two wires, which deliver the checkbox GUI element functionality.
checkBoxW :: GUIElement -- ^ GUI element (should be a checkbox)
             -> (GameWire a Bool, GameWire Bool Bool) -- ^ the returned wires, a value changed wire and a setter wire
checkBoxW widget = (changed, setter)  where
  (changed', setter') = _createGUIValueW widget "Selected" "CheckStateChanged"
  changed = _stb . changed'
  setter = _stb . setter' . _bst
  
-- | Constructor for two wires, which deliver the radiobuttion GUI element functionality.
radioButtonW :: GUIElement -- ^ GUI element (should be a checkbox)
                -> (GameWire a Bool, GameWire Bool Bool) -- ^ the returned wires, a value changed wire and a setter wire
radioButtonW  widget = (changed, setter)  where
  (changed', setter') = _createGUIValueW widget "Selected" "SelectStateChanged"
  changed = _stb . changed'
  setter = _stb . setter' . _bst

_fst = mkFix (\time f -> Right $ show f)
_stf = mkFix (\time st -> Right $ read st)
  
-- | Constructor for two wires, which deliver the slider GUI element functionality.
sliderW :: GUIElement -- ^ GUI element (should be a slider)
           -> (GameWire a Float, GameWire Float Float) -- ^ the returned wires, a value changed wire and a setter wire
sliderW widget = (changed, setter) where
  (changed', setter') = _createGUIValueW widget "CurrentValue" "ValueChanged"
  changed = _stf . changed'
  setter = _stf . setter' . _fst

-- | Constructur for two wires, which deliver the spinner GUI element functionality.
spinnerW :: GUIElement -- ^ GUI element (which should be a spinner)
            -> (GameWire a Float, GameWire Float Float) -- ^ the returned wires, a value changed wire and a setter wire
spinnerW = sliderW

-- | Constructure for an event wire, which deliver the button functionality. It simply fires the event on button press.
buttonW :: GUIElement -- ^ GUI element (which should be a button)
           -> GameWire a a -- ^ event wire
buttonW widget = guiEventW widget "Clicked" 

_listboxGetSelectedTextW :: GUIElement -> GameWire a [String]
_listboxGetSelectedTextW widget = mkFixM (\t s -> do
                                         t <- listboxGetSelectedText widget
                                         return $ Right t)

_listboxSetTextW :: GUIElement -> GameWire [String] [String]
_listboxSetTextW widget = mkFixM (\t ls -> do
                                    listboxRemoveAllText widget
                                    mapM (listboxAddText widget) ls
                                    return (Right ls))

-- | Constructur for two wires, which deliver the listbox functionality.
listBoxW :: GUIElement -- ^ GUI element, which should be a list box
            -> (GameWire a [String], GameWire  [String] [String]) -- ^ Two wires as return value, a value changed wire and a wire
            -- which sets the listbox. Those wires are slightly differently since the setter sets the list of choices, not the 
            -- actual selection on them. The changed value wire delivers an array of selections on each change.
listBoxW  widget = (changed, setter) where
  changeW = guiEventW widget "ItemSelectionChanged"
  valueW = _listboxGetSelectedTextW widget
  changed = valueW . changeW
  setter = _listboxSetTextW widget
	
_comboboxSetTextW :: GUIElement -> GameWire [String] [String]
_comboboxSetTextW widget = mkFixM (\t ls -> do
                                    comboboxRemoveAllText widget
                                    mapM (comboboxAddText widget) ls
                                    return (Right ls))

-- | Constructor for two wires, which deliver the combobox functionality.
comboBoxW :: GUIElement -- ^ GUI element, which should be a combobox
             -> (GameWire a String, GameWire  [String] [String]) -- ^ Two wires, one value changed wire, which delivers just one selection and a setter wire, with which the selectable elements can be set. The possibility, to edit the selected element is not included in the functionality.
comboBoxW widget = (changed, setter) where
  changeW = guiEventW widget "ListSelectionAccepted"
  valueW = guiGetPropW widget "Text"
  changed = valueW . changeW
  setter = _comboboxSetTextW widget


_changeOnlyOnInputWire (cw, sw) = do
  rb <- liftIO $ newIORef False
  let sw' = mkFixM (\t a -> do
                       liftIO $ writeIORef rb True
                       return (Right a)) . sw
  let cw' = mkFixM (\t a -> do
                       b <- liftIO $ readIORef rb
                       liftIO $ writeIORef rb False
                       if b then return (Left ()) else return (Right a)) . cw
  return (cw', sw')

	
-- | Constructor for two wires, which in combination provide the functionality of an editable box GUI element. Version, which does not deliver a change event, if value set by program.
editBoxW' :: GUIElement -- ^ GUI element (should be an editbox)
            -> MHGamer3D (GameWire a String, GameWire String String) -- ^ Two wires as return value, a wire, which fires when the value changed and a setter wire, which sets the text of the editbox.
editBoxW' el = _changeOnlyOnInputWire (editBoxW el)

-- | Constructor for two wires, which provide the functionality of an editable box for float values as a GUI element. Version, which does not deliver a change event, if value set by program.
floatEditBoxW' :: GUIElement -- ^ GUI element (should be an editbox)
                 -> MHGamer3D (GameWire a Float, GameWire Float Float) -- ^ Two wires as return value, a value changed wire and a setter wire.
floatEditBoxW' el = _changeOnlyOnInputWire (floatEditBoxW el)

-- | Consructor for two wires, which deliver the checkbox GUI element functionality. Version, which does not deliver a change event, if value set by program.
checkBoxW' :: GUIElement -- ^ GUI element (should be a checkbox)
             -> MHGamer3D (GameWire a Bool, GameWire Bool Bool) -- ^ the returned wires, a value changed wire and a setter wire
checkBoxW' el = _changeOnlyOnInputWire (checkBoxW el)

-- | Constructor for two wires, which deliver the radiobuttion GUI element functionality. Version, which does not deliver a change event, if value set by program.
radioButtonW' :: GUIElement -- ^ GUI element (should be a checkbox)
                -> MHGamer3D (GameWire a Bool, GameWire Bool Bool) -- ^ the returned wires, a value changed wire and a setter wire
radioButtonW' el = _changeOnlyOnInputWire (radioButtonW el)

-- | Constructor for two wires, which deliver the slider GUI element functionality. Version, which does not deliver a change event, if value set by program.
sliderW' :: GUIElement -- ^ GUI element (should be a slider)
           -> MHGamer3D (GameWire a Float, GameWire Float Float) -- ^ the returned wires, a value changed wire and a setter wire
sliderW' el = _changeOnlyOnInputWire (sliderW el)

-- | Constructur for two wires, which deliver the spinner GUI element functionality. Version, which does not deliver a change event, if value set by program.
spinnerW' :: GUIElement -- ^ GUI element (which should be a spinner)
            -> MHGamer3D (GameWire a Float, GameWire Float Float) -- ^ the returned wires, a value changed wire and a setter wire
spinnerW' el = _changeOnlyOnInputWire (spinnerW el)


{-$EditGUIWire
  The standard GUI wire creation functions are not in the MHGamer3D monad. This enables their usage in an easy way in GHCI.
  The editable elements are composed of two different types of functions, which might occurr at different times and therefore
  the corresponding wires are two, in each case. One type of functions sets the content of the editable element. For example 
  the setter functions set the text of an editbox or the position of a slider. The other type of functions deliver an event
  in the case the value has been changed from the GUI user. The simple wires below also deliver a change event in case
  the value has been set by the programmer with the setter. Below are more complex wires (still with the same API) which do
  not deliver a change event, in case the programmer sets the value of a GUI element.
-}

{-$FireOnlyOnGUI
  Below functions do not deliver a change event in case the programmer sets the value by the setter wire. This enables much more
  easy usage in case of circular dependencies between GUI elements. The creation functions are in the MHGamer3D monad.
-}