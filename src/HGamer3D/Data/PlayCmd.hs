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
import Data.Binary.Serialise.CBOR.Encoding
import Data.Binary.Serialise.CBOR.Decoding

import Data.Text
import Data.Monoid
import Control.Applicative


data PlayCmd = Play
    | Pause
    | Stop
    deriving (Eq, Read, Show)

ctPlayCmd :: ComponentType PlayCmd
ctPlayCmd = ComponentType 0x35f7752020f7f1cd

instance Serialise PlayCmd where
    encode (Play) = encodeListLen 1 <>  encode (0::Int)
    encode (Pause) = encodeListLen 1 <>  encode (1::Int)
    encode (Stop) = encodeListLen 1 <>  encode (2::Int)
    decode = do
        decodeListLen
        i <- decode :: Decoder s Int
        case i of
            0 -> (pure Play)
            1 -> (pure Pause)
            2 -> (pure Stop)




