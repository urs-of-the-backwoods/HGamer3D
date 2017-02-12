{-
	Low, Medium, High Datatype
	HGamer3D Library (A project to enable 3D game development in Haskell)
	Copyright 2011-2017 Peter Althainz
	
	Distributed under the Apache License, Version 2.0
	(See attached file LICENSE or copy at 
	http://www.apache.org/licenses/LICENSE-2.0)
 
	file: HGamer3D/Data/LMH.hs
-}

-- | Data type to specify a 3 choice volume of anything: Low, Medium, High
module HGamer3D.Data.LMH
where 

import Fresco
import Data.Binary.Serialise.CBOR
import Data.Binary.Serialise.CBOR.Decoding

import Data.Text
import Data.Monoid
import Control.Applicative


type Switch = Bool

ctSwitch :: ComponentType Switch
ctSwitch = ComponentType 0x30b235f8b63df8b0

data LMH = Low
    | Medium
    | High
    deriving (Eq, Read, Show)

type QualityLMH = LMH

ctQualityLMH :: ComponentType QualityLMH
ctQualityLMH = ComponentType 0xd632bb5447a6c93c

instance Serialise LMH where
    encode (Low) = encode (0::Int) 
    encode (Medium) = encode (1::Int) 
    encode (High) = encode (2::Int) 
    decode = do
        i <- decode :: Decoder Int
        case i of
            0 -> (pure Low)
            1 -> (pure Medium)
            2 -> (pure High)
