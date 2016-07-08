{-
	Low, Medium, High Datatype
	HGamer3D Library (A project to enable 3D game development in Haskell)
	Copyright 2011-2015 Peter Althainz
	
	Distributed under the Apache License, Version 2.0
	(See attached file LICENSE or copy at 
	http://www.apache.org/licenses/LICENSE-2.0)
 
	file: HGamer3D/Data/LMH.hs
-}

-- | Data type to specify a 3 choice volume of anything: Low, Medium, High
module HGamer3D.Data.LMH
(
    ctOnOff,
    QualityLMH (..),
    ctLMH
)

where

import Data.MessagePack
import Fresco

ctOnOff :: ComponentType Bool
ctOnOff = ComponentType 0x30b235f8b63df8b0

data QualityLMH = Low
    | Medium
    | High
    deriving (Eq, Read, Show)

instance ComponentClass QualityLMH where
    toObj (Low) = ObjectArray [ObjectInt 0, ObjectArray []]
    toObj (Medium) = ObjectArray [ObjectInt 1, ObjectArray []]
    toObj (High) = ObjectArray [ObjectInt 2, ObjectArray []]
    fromObj (ObjectArray [ObjectInt 0, ObjectArray []]) = Low
    fromObj (ObjectArray [ObjectInt 1, ObjectArray []]) = Medium
    fromObj (ObjectArray [ObjectInt 2, ObjectArray []]) = High


ctLMH :: ComponentType QualityLMH
ctLMH = ComponentType 0xd632bb5447a6c93c



