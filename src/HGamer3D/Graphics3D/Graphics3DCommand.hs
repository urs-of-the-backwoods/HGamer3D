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
where

import Fresco
import Data.Binary.Serialise.CBOR
import Data.Binary.Serialise.CBOR.Decoding

import Data.Text
import Data.Monoid
import Control.Applicative


data Graphics3DCommand = NoCmd
    | Step
    deriving (Eq, Read, Show)


instance Serialise Graphics3DCommand where
    encode (NoCmd) = encode (0::Int) 
    encode (Step) = encode (1::Int) 
    decode = do
        i <- decode :: Decoder Int
        case i of
            0 -> (pure NoCmd)
            1 -> (pure Step)

ctGraphics3DCommand :: ComponentType Graphics3DCommand
ctGraphics3DCommand = ComponentType 0xea06bc20a9334af3

