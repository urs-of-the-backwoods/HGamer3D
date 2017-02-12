{-
	The Play Command
	HGamer3D Library (A project to enable 3D game development in Haskell)
	Copyright 2011 - 2017 Peter Althainz
	
	Distributed under the Apache License, Version 2.0
	(See attached file LICENSE or copy at 
	http://www.apache.org/licenses/LICENSE-2.0)
 
	file: HGamer3D/Data/PlayCmd.hs
-}

-- | Type for a generic play command
module HGamer3D.Data.PlayCmd
where

import Fresco
import Data.Binary.Serialise.CBOR
import Data.Binary.Serialise.CBOR.Decoding

import Data.Text
import Data.Monoid
import Control.Applicative


data PlayCmd = Play
    | Pause
    | Stop
    deriving (Eq, Read, Show)


instance Serialise PlayCmd where
    encode (Play) = encode (0::Int) 
    encode (Pause) = encode (1::Int) 
    encode (Stop) = encode (2::Int) 
    decode = do
        i <- decode :: Decoder Int
        case i of
            0 -> (pure Play)
            1 -> (pure Pause)
            2 -> (pure Stop)

ctPlayCmd :: ComponentType PlayCmd
ctPlayCmd = ComponentType 0x35f7752020f7f1cd



