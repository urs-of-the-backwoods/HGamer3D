module HGamer3D.GUI.FontSize
where
	
import Fresco
import Data.Binary.Serialise.CBOR
import Data.Binary.Serialise.CBOR.Encoding
import Data.Binary.Serialise.CBOR.Decoding

import Data.Text
import Data.Monoid
import Control.Applicative


type FontSize = Int

ctFontSize :: ComponentType FontSize
ctFontSize = ComponentType 0x829863cdd141007e


