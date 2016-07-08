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

-- HGamer3D/Graphics3D/Graphics3DCommand.hs

-- | Module providing the CmdGraphics3D type
module HGamer3D.Graphics3D.Graphics3DCommand

(
        Graphics3DCommand (..),
        ctGraphics3DCommand
)

where

import Data.MessagePack
import Fresco
import HGamer3D.Data

-- | Component Type of Graphics3DCommand
ctGraphics3DCommand :: ComponentType Graphics3DCommand
ctGraphics3DCommand = ComponentType 0xea06bc20a9334af3

-- OUTPUT SINOPIA START HERE

-- | Commands, which can be sent to graphics system
data Graphics3DCommand = NoCmd
    | Step
    deriving (Eq, Read, Show)

instance ComponentClass Graphics3DCommand where
    toObj (NoCmd) = ObjectArray [ObjectInt 0, ObjectArray []]
    toObj (Step) = ObjectArray [ObjectInt 1, ObjectArray []]
    fromObj (ObjectArray [ObjectInt 0, ObjectArray []]) = NoCmd
    fromObj (ObjectArray [ObjectInt 1, ObjectArray []]) = Step

-- OUTPUT SINOPIA ENDS HERE


