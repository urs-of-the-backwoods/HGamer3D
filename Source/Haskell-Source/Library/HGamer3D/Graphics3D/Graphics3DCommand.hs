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
import HGamer3D.Data

-- | Commands, which can be sent to graphics system
data Graphics3DCommand = NoCmd 
					| Step 
					deriving (Eq, Show)

-- | Component Type of Graphics3DCommand
ctGraphics3DCommand :: ComponentType Graphics3DCommand
ctGraphics3DCommand = ComponentType 0xea06bc20a9334af3

instance ComponentClass Graphics3DCommand where
	toObj c = case c of
		NoCmd -> ObjectArray [ObjectInt 0]
		Step -> ObjectArray [ObjectInt 1]

	fromObj c = case c of
		(ObjectArray [ObjectInt 0]) -> NoCmd
		(ObjectArray [ObjectInt 1]) -> Step

