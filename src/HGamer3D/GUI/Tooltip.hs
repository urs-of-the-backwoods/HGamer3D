module HGamer3D.GUI.Tooltip
where

import Fresco
import Data.Binary.Serialise.CBOR
import Data.Binary.Serialise.CBOR.Encoding
import Data.Binary.Serialise.CBOR.Decoding

import Data.Text
import Data.Monoid
import Control.Applicative


-- | pop up, which explains something
type Tooltip = Text

ctTooltip :: ComponentType Tooltip
ctTooltip = ComponentType 0x620bb5dd7dfca052


