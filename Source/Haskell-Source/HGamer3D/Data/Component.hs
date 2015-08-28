-- This source file is part of HGamer3D
-- (A project to enable 3D game development in Haskell)
-- For the latest info, see http://www.althainz.de/HGamer3D.html
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

-- HGamer3D/Data/Component.hs

{-# LANGUAGE TypeSynonymInstances #-}

-- | Components of the Entity Component System of HGamer3D
module HGamer3D.Data.Component
(
	ComponentType (..),
	Component,
	ComponentClass (..)
)
where

import Data.Word
import Data.MessagePack
import Data.ByteString
import Data.Text
import Data.Text.Encoding

-- | Components in Entities are indexed by ComponentType
data ComponentType a = ComponentType Word64

-- | Components are stored as ByteString
type Component = ByteString

-- | ComponentClass is the typeclass of data types, which can be components
class ComponentClass a where
  toObj :: a -> Object
  fromObj :: Object -> a

instance ComponentClass () where
   toObj () = ObjectNil
   fromObj ObjectNil = ()

instance ComponentClass Text where
   toObj text = ObjectString (encodeUtf8 text)
   fromObj (ObjectString bs) = decodeUtf8 bs 

instance ComponentClass (ComponentType a) where
   toObj (ComponentType i) = ObjectInt (fromIntegral i)
   fromObj (ObjectInt i) = ComponentType (fromIntegral i)


