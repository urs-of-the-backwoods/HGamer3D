module HGamer3D.GUI.Font
where
	
import Fresco
import Data.Binary.Serialise.CBOR
import Data.Binary.Serialise.CBOR.Encoding
import Data.Binary.Serialise.CBOR.Decoding

import Data.Text
import Data.Monoid
import Control.Applicative


type Font = Text

ctFont :: ComponentType Font
ctFont = ComponentType 0x457ac00afe66a3a4


