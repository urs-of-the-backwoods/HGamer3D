{-
    Light Datatype
	HGamer3D Library (A project to enable 3D game development in Haskell)
	Copyright 2011-2015 Peter Althainz
	
	Distributed under the Apache License, Version 2.0
	(See attached file LICENSE or copy at 
	http://www.apache.org/licenses/LICENSE-2.0)
 
	file: HGamer3D/Graphics3D/Light.hs
-}

-- | Module providing the Light type
module HGamer3D.Graphics3D.Light

(
    LightType (..),
    Light (..),
    ctLight
)

where

import Data.MessagePack
import Fresco
import HGamer3D.Data

-- HGamer3D website, entities, example 2 for data types

data LightType =    PointLight              -- ^ casting light in all directions, from position 
                    | DirectionalLight      -- ^ like a very far light source (the Sun)
                    | SpotLight Angle Float -- ^ a light with a field of view (Angle) and an aspect-ratio (Float)
                
instance ComponentClass LightType where
    toObj PointLight = ObjectArray [ObjectInt 0]
    toObj DirectionalLight =  ObjectArray [ObjectInt 1]
    toObj (SpotLight fov ar) = ObjectArray [ObjectInt 2, toObj fov, ObjectFloat ar]

    fromObj (ObjectArray [ObjectInt 0]) = PointLight
    fromObj (ObjectArray [ObjectInt 1]) = DirectionalLight 
    fromObj (ObjectArray [ObjectInt 2, fov_o, ObjectFloat ar]) = SpotLight (fromObj fov_o) ar

data Light = Light 
                LightType 
                Float     
                Float     
                Float     -- ^ floats: brightness, range, specular intensity

instance ComponentClass Light where
    toObj (Light lt b r s) = ObjectArray [toObj lt, ObjectFloat b, ObjectFloat r, ObjectFloat s]
    fromObj (ObjectArray [lt_o, ObjectFloat b, ObjectFloat r, ObjectFloat s]) = Light (fromObj lt_o) b r s
    
ctLight :: ComponentType Light
ctLight = ComponentType 0x981e80e50d994ea9

-- end of website text

