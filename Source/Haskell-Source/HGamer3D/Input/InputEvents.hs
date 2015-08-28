-- This source file is part of HGamer3D
-- (A project to enable 3D game development in Haskell)
-- For the latest info, see http://www.hgamer3d.org
--
-- (c) 2011 - 2015 Peter Althainz
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

-- HGamer3D/Input/InputEvents.hs

-- | Module providing the MouseInput type

module HGamer3D.Input.InputEvents

(
	ctInputEventHub,
	ctMouseEvent,

  MouseEvent (..)
)

where

import Data.MessagePack
import Debug.Trace
import Data.Text

import HGamer3D.Data

ctInputEventHub :: ComponentType ()
ctInputEventHub = ComponentType 0xa532f43b1c1c6bc7

ctMouseEvent :: ComponentType MouseEvent
ctMouseEvent = ComponentType 0x27eaf3fd46595d08

data MouseEvent = MousePosition Int Int
		  | MouseLeftClick
                  | MouseRightClick
                  deriving (Eq, Show)

instance ComponentClass MouseEvent where
        toObj (MousePosition x y) = ObjectArray [ObjectInt 1, ObjectInt (fromIntegral x), ObjectInt (fromIntegral y)]
        toObj MouseLeftClick = ObjectArray [ObjectInt 2]
        toObj MouseRightClick = ObjectArray [ObjectInt 3]
        fromObj (ObjectArray [ObjectInt 1, ObjectInt x, ObjectInt y]) = MousePosition (fromIntegral x) (fromIntegral y)
        fromObj (ObjectArray [ObjectInt 2]) = MouseLeftClick
        fromObj (ObjectArray [ObjectInt 3]) = MouseRightClick
 