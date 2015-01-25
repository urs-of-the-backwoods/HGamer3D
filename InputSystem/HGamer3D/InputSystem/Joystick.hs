{-# LANGUAGE DeriveDataTypeable, StandaloneDeriving #-}
-- This source file is part of HGamer3D
-- (A project to enable 3D game development in Haskell)
-- For the latest info, see http://www.hgamer3d.org
--
-- (c) 2015 Peter Althainz
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

module HGamer3D.InputSystem.Joystick
(
        Joystick (..),
        JoystickButton (..),
        EnumJoystickAxis (..),
        JoystickInfo (..),
        JoystickValue (..)
)
where

import Data.Typeable
import HGamer3D.BaseAPI.InputSystem

deriving instance Typeable Joystick
deriving instance Typeable JoystickButton
deriving instance Typeable EnumJoystickAxis

data JoystickInfo = JoystickInfo Joystick [EnumJoystickAxis] [JoystickButton]
                  deriving (Eq, Show, Typeable)

data JoystickValue = JoystickValue Bool [Float] [Bool]
                  deriving (Eq, Show, Typeable)

