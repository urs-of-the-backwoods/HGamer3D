{-
	Components are pieces of the handcrafted record system
	HGamer3D Library (A project to enable 3D game development in Haskell)
	Copyright 2015 Peter Althainz
	
	Distributed under the Apache License, Version 2.0
	(See attached file LICENSE or copy at 
	http://www.apache.org/licenses/LICENSE-2.0)
 
	file: HGamer3D/Data/Component.hs
-}
    
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
data ComponentType a = ComponentType Word64 deriving (Eq, Show, Ord)

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
   
instance ComponentClass Bool where
    toObj b = ObjectBool b
    fromObj (ObjectBool b) = b


