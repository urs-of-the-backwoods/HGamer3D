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
    LMH (..),
    ctLMH
)

where

import Data.MessagePack
import HGamer3D.Data.Component

ctOnOff :: ComponentType Bool
ctOnOff = ComponentType 0x30b235f8b63df8b0

data LMH = Low
        |  Medium
        |  High
        deriving (Eq, Ord, Show)

instance ComponentClass LMH where
        toObj lmh = case lmh of
                Low -> ObjectInt 1
                Medium -> ObjectInt 2
                High -> ObjectInt 3
        fromObj (ObjectInt n) = case n of
                1 -> Low
                2 -> Medium
                3 -> High

ctLMH :: ComponentType LMH
ctLMH = ComponentType 0xd632bb5447a6c93c



