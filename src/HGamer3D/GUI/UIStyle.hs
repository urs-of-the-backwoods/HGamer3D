module HGamer3D.GUI.UIStyle
where

import Fresco
import Data.Binary.Serialise.CBOR
import Data.Binary.Serialise.CBOR.Encoding
import Data.Binary.Serialise.CBOR.Decoding

import Data.Text
import Data.Monoid
import Control.Applicative


-- | a specific UI Style, as a name of a resource or similar
type UIStyle = Text

ctUIStyle :: ComponentType UIStyle
ctUIStyle = ComponentType 0xbd86f89ddca9280f


