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

-- HGamer3D/Data/LMH.hs

-- | Low Medium High qualification
module HGamer3D.Data.LMH

(
        LMH (..)
)

where

import Data.MessagePack
import HGamer3D.Data.Component

data LMH = Disabled
        |  Low
        |  Medium
        |  High

instance ComponentClass LMH where
        toObj lmh = case lmh of
                Disabled -> ObjectInt 0
                Low -> ObjectInt 1
                Medium -> ObjectInt 2
                High -> ObjectInt 3
        fromObj (ObjectInt n) = case n of
                0 -> Disabled
                1 -> Low
                2 -> Medium
                3 -> High

