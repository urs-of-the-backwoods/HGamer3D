module HGamer3D.GUI.WindowGUI
where

import Fresco
import Data.Binary.Serialise.CBOR
import Data.Binary.Serialise.CBOR.Encoding
import Data.Binary.Serialise.CBOR.Decoding

import Data.Text
import Data.Monoid
import Control.Applicative


-- | 2D area on the screen, which can be moved, ...
type WindowGUI = ()

ctWindowGUI :: ComponentType WindowGUI
ctWindowGUI = ComponentType 0x39b4f64b33f5cb41


