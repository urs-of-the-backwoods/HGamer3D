module HGamer3D.GUI.Texture
where

import Fresco
import Data.Binary.Serialise.CBOR
import Data.Binary.Serialise.CBOR.Encoding
import Data.Binary.Serialise.CBOR.Decoding

import Data.Text
import Data.Monoid
import Control.Applicative


-- | name of a texture, as a resource
type Texture = Text

ctTexture :: ComponentType Texture
ctTexture = ComponentType 0xb28a202495ec05b1


