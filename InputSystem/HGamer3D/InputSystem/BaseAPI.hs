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

-- InputSystem.hs


-- | Mouse, Keyboard and Joystick functionality for HGamer3D, public API.

module HGamer3D.InputSystem.BaseAPI

(
	-- * Types
	module HGamer3D.Bindings.SFML.EnumJoystickAxis,
	module HGamer3D.Bindings.SFML.EnumKey,
	module HGamer3D.Bindings.SFML.EnumMouseButton,
	
	Joystick,
	JoystickButton,
	
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

import qualified HGamer3D.Bindings.SFML.ClassJoystick as JST

import qualified HGamer3D.Bindings.SFML.ClassKeyboard as Keyboard

import qualified HGamer3D.Bindings.SFML.ClassMouse as Mouse
import qualified HGamer3D.Bindings.SFML.ClassMouseHG3D as Mouse2

import HGamer3D.Bindings.SFML.EnumJoystickAxis
import HGamer3D.Bindings.SFML.EnumKey
import HGamer3D.Bindings.SFML.EnumMouseButton

import Control.Monad

import HGamer3D.InputSystem.Internal.Base

