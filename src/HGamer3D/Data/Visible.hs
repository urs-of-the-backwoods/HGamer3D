{-
	Visible Datatype
	HGamer3D Library (A project to enable 3D game development in Haskell)
	Copyright 2011-2017 Peter Althainz

	Distributed under the Apache License, Version 2.0
	(See attached file LICENSE or copy at 
	http://www.apache.org/licenses/LICENSE-2.0)

	file: HGamer3D/Data/Visible.hs
-}

-- | Data type to specify if something is visible
module HGamer3D.Data.Visible
where 

import Fresco
import Data.Binary.Serialise.CBOR
import Data.Binary.Serialise.CBOR.Encoding
import Data.Binary.Serialise.CBOR.Decoding

import Data.Text
import Data.Monoid
import Control.Applicative


type Visible = Bool

ctVisible :: ComponentType Visible
ctVisible = ComponentType 0x98e7a78e949e1c6e
