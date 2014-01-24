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

-- Vector2.hs

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- book: math3d vector lib
-- |Includes the right pieces of the Vect libray of Balazs Komuves.
-- see: <http://hackage.haskell.org/package/vect>

module HGamer3D.Data.Vector.Instances
(
	module Data.Vect.Float.Instances,
)

where

import Data.Vect.Float.Instances
-- book: end
