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

-- InputSystem.hs


-- | Mouse, Keyboard and Joystick functionality of the Base API.
module HGamer3D.APIs.Base.InputSystem.InputSystem

(
	-- * Types
	module HGamer3D.Bindings.SFML.EnumJoystickAxis,
	module HGamer3D.Bindings.SFML.EnumKey,
	module HGamer3D.Bindings.SFML.EnumMouseButton,
	
	Joystick (..),
	JoystickButton (..),
	
	-- * Joystick Functions
	updateJoystickStatus,
	
	getConnectedJoysticks,
	isJoystickConnected,
	
	getJoystickAxes,
	getJoystickButtons,
	
	isJoystickButtonPressed,
	getJoystickAxisPosition,
	
	-- * Keyboard Functions
	isKeyPressed,
	
	-- * Mouse Functions
	isMouseButtonPressed,
	getMousePosition,
	
	
	
)

where

import GHC.Ptr

import HGamer3D.Data.HG3DClass
import HGamer3D.Data.Vector

import HGamer3D.Bindings.SFML.ClassPtr
import HGamer3D.Bindings.SFML.Utils

import qualified HGamer3D.Bindings.SFML.ClassJoystick as Joystick
import qualified HGamer3D.Bindings.SFML.ClassKeyboard as Keyboard
import qualified HGamer3D.Bindings.SFML.ClassMouse as Mouse
import qualified HGamer3D.Bindings.SFML.ClassMouseHG3D as Mouse2
import HGamer3D.Bindings.SFML.EnumJoystickAxis
import HGamer3D.Bindings.SFML.EnumKey
import HGamer3D.Bindings.SFML.EnumMouseButton

import HGamer3D.APIs.Base.Engine.Types

import Control.Monad.Trans
import Control.Monad.Reader


-- | The Joystick
data Joystick = Joystick Int deriving (Eq)
data JoystickButton = JoystickButton Int deriving (Eq)

instance Show Joystick where
	show (Joystick j) = show $ "Joystick-" ++ (show j)

instance Show JoystickButton where
	show (JoystickButton jb) = show $ "JoystickButton-" ++ (show jb)

instance Show EnumJoystickAxis where
	show axis = "JoystickAxis-" ++ (case axis of
		JoystickAxisX -> "X"
		JoystickAxisY -> "Y"
		JoystickAxisZ -> "Z"
		JoystickAxisR -> "R"
		JoystickAxisU -> "U"
		JoystickAxisV -> "V"
		JoystickAxisPovX -> "PovX"
		JoystickAxisPovY -> "PoxY"
		)
	

-- Joystick funtions
--

-- | This functions gathers the Joystick inputs. It should called frequently, for example during the gameloop, to get new values and new Joystick availability. At least it should be called before querying the Joystick values.
updateJoystickStatus :: MHGamer3D ()
updateJoystickStatus = liftIO Joystick.update

-- | Get a list of connected Joysticks.
getConnectedJoysticks :: MHGamer3D [Joystick] -- ^ returns list of connected Joysticks
getConnectedJoysticks = do
	let ns = [ x | x <- [0..15]]
	jns <- filterM (\jn -> do
		c <- liftIO $ Joystick.isConnected jn
		return (c) ) ns
	let js = map Joystick jns
	return js
	
-- | Queries, if a specific Joystick is connected.
isJoystickConnected :: Joystick -- ^ the Joystick to be queried
                       -> MHGamer3D Bool -- ^ True: Joystick is connected
isJoystickConnected (Joystick jn) = do
	rv <- liftIO $ Joystick.isConnected jn
	return rv
	
-- | Queries the available axes of one specific Joystick
getJoystickAxes:: Joystick -- ^ the Joystick to be queried
                  -> MHGamer3D [EnumJoystickAxis] -- ^ returns a list of available axes
getJoystickAxes (Joystick j) = do
	axes <- filterM ( \a -> do
		liftIO $ Joystick.hasAxis j a) [ JoystickAxisX, JoystickAxisY, JoystickAxisZ, 
			JoystickAxisR, JoystickAxisU, JoystickAxisV, 
			JoystickAxisPovX, JoystickAxisPovY ]
	return axes

-- | Queries the available buttons of one specific Joystick
getJoystickButtons :: Joystick -- ^ the Joystick to be queried
                      -> MHGamer3D [JoystickButton] -- ^ returns a list of available buttons
getJoystickButtons (Joystick j) = do
	jn <- liftIO $ Joystick.getButtonCount j
	let btns = map JoystickButton [0..(jn-1)]
	return btns

-- | Check if a specific Joystick Button is currently pressed
isJoystickButtonPressed :: Joystick -- ^ the Joystick 
                           -> JoystickButton -- ^ the Button
                           -> MHGamer3D Bool -- ^ returns True if pressed
isJoystickButtonPressed (Joystick j) (JoystickButton b) = liftIO $ Joystick.isButtonPressed j b

-- | Queries the current position of a Joystick Axis
getJoystickAxisPosition :: Joystick -- ^ the Joystick
                           -> EnumJoystickAxis -- ^ the Axis
                           -> MHGamer3D Float -- ^ returns the postion, ranging from -x.0 to x.0, x depending on Joystick
getJoystickAxisPosition (Joystick j) ax = liftIO $ Joystick.getAxisPosition j ax


-- Keyboard functions
--

-- | Check if a specific keyboard key is currently pressed
isKeyPressed :: EnumKey -- ^ the key
                -> MHGamer3D Bool -- ^ True, if pressed
isKeyPressed key = liftIO $ Keyboard.isKeyPressed key

-- Mouse functions
--

-- | Check if a specific mouse button is pressed
isMouseButtonPressed :: EnumMouseButton  -- ^ mouse button
                        -> MHGamer3D Bool -- ^ True, if pressed
isMouseButtonPressed mb = liftIO $ Mouse.isButtonPressed mb

-- | Queries current mouse position
getMousePosition :: MHGamer3D (Int, Int) -- ^ absolute mouse position on screen in pixel
getMousePosition = liftIO Mouse2.getPosition


