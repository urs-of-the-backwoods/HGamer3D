{-
	GUI: DropDownList functionality
	HGamer3D Library (A project to enable 3D game development in Haskell)
	Copyright 2015 Peter Althainz
	
	Distributed under the Apache License, Version 2.0
	(See attached file LICENSE or copy at 
	http://www.apache.org/licenses/LICENSE-2.0)
 
	file: HGamer3D/GUI/DropDownList.hs
-}

-- | Module providing the Mouse functionality and settings
module HGamer3D.GUI.DropDownList
where

import Fresco
import Data.Binary.Serialise.CBOR
import Data.Binary.Serialise.CBOR.Decoding

import Data.Text
import Data.Monoid
import Control.Applicative


data DropDownList = DropDownList {
    dropDownListContent::[Text],
	dropDownListSelected::(Maybe Int)
    } deriving (Eq, Read, Show)

ctDropDownList :: ComponentType DropDownList
ctDropDownList = ComponentType 0x200de0e837a8e590

instance Serialise DropDownList where
    encode (DropDownList v1 (Just v2)) = encode v1 <> encode (0::Int) <> encode v2
    encode (DropDownList v1 Nothing) = encode v1 <> encode (0::Int)
    decode = DropDownList <$> decode <*> decodeMI where
		decodeMI = do
			i <- decode :: Decoder Int
			case i of
				0 -> (Just <$> decode)
				1 -> (pure Nothing)




