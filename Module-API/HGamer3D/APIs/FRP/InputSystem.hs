{-# LANGUAGE Arrows #-}

-- | Input device functionality of FRP API. Mouse, Joystick, Keyboard input is handled.
module HGamer3D.APIs.FRP.InputSystem

(
  -- * Joystick functionality
  connectedJoysticksW,
  joystickButtonW,
  joystickButtonPressedW,
  joystickAxisPositionW,
  
  -- * Mouse functionality
  mouseButtonW,
  mouseButtonPressedW,
  mousePositionW,
  
  -- * Keyboard functionality
  keyW,
  keyPressedW
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

-- Joystick
-----------

-- | Wire, which delivers the connected joysticks
connectedJoysticksW :: GameWire a [Joystick]
connectedJoysticksW = mkFixM (\t a -> do
                                 updateJoystickStatus
                                 js <- getConnectedJoysticks
                                 return (Right js))

_joystickButtonStatus :: Joystick -> JoystickButton -> GameWire a Bool
_joystickButtonStatus joystick button = mkFixM (\t a -> do
                               updateJoystickStatus   
                               pressed <- isJoystickButtonPressed joystick button
                               return (Right pressed))
                                  
-- | Wire, which fires, when joystick button is pressed
joystickButtonW :: Joystick -- ^ joystick
                   -> JoystickButton -- ^ joystick button, to be checked
                   -> GameWire a a -- ^ event, which fires, when button pressed
joystickButtonW joystick button = ifW (_joystickButtonStatus joystick button) id (inhibit ())

-- | Wire, which fires after a button down, button up sequence of a joystick button
joystickButtonPressedW :: Joystick -- ^ joystick
                          -> JoystickButton -- ^ joystick button, to be checked
                          -> GameWire a a -- ^ event, which fires, after button has been pressed once
joystickButtonPressedW joystick button = ifW ((edge (\a -> not a)) . (_joystickButtonStatus joystick button)) id (inhibit ())

-- | Wire, which delivers the value of a joystick axis
joystickAxisPositionW :: Joystick -- ^ joystick
                         -> EnumJoystickAxis -- ^ joystick axis, to be checked
                         -> GameWire a Float -- ^ delivers position of joystick axis, input value ignored
joystickAxisPositionW joystick axis = mkFixM (\t a -> do
                                             updateJoystickStatus
                                             rval <- getJoystickAxisPosition joystick axis
                                             return (Right rval))

                         
-- Mouse
--------

_mouseButtonStatus :: EnumMouseButton -> GameWire a Bool
_mouseButtonStatus button = mkFixM (\t a -> do
                               pressed <- isMouseButtonPressed button
                               return (Right pressed))
                                  
-- | Wire, which fires, when mouse button is pressed
mouseButtonW ::  EnumMouseButton -- ^ mouse button, to be checked
                   -> GameWire a a -- ^ event, which fires, when button pressed
mouseButtonW button = ifW (_mouseButtonStatus button) id (inhibit ())

-- | Wire, which fires after a button down, button up sequence of a mouse button
mouseButtonPressedW :: EnumMouseButton -- ^ mouse button, to be checked
                          -> GameWire a a -- ^ event, which fires, after button has been pressed once
mouseButtonPressedW button = ifW ((edge (\a -> not a)) . (_mouseButtonStatus button)) id (inhibit ())

-- | Wire, which delivers the value of the mouse position
mousePositionW :: GameWire a (Int, Int) -- ^ delivers position of mouse, x, y in absolute screen pixels, input value ignored
mousePositionW  = mkFixM (\t a -> do
                                             rval <- getMousePosition
                                             return (Right rval))



-- Keyboard
-----------

_keyStatus :: EnumKey -> GameWire a Bool
_keyStatus key = mkFixM (\t a -> do
                                val <- isKeyPressed key
                                return (Right val))
                 
-- | Wire, which delivers an event, if key is pressed
keyW :: EnumKey -- ^ the key, to be checked
        -> GameWire a a -- ^ the event wire
keyW key = ifW (_keyStatus key) id (inhibit ())

-- | Wire, which delivers an event after a key down, key up sequence
keyPressedW :: EnumKey -- ^ the key, to be checked
               -> GameWire a a -- ^ the event wire
keyPressedW key = ifW ((edge (\a -> not a)) . (_keyStatus key)) id (inhibit ())



