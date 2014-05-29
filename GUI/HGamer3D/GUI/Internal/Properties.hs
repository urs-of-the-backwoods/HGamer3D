-- This source file is part of HGamer3D
-- (A project to enable 3D game development in Haskell)
-- For the latest info, see http://www.hgamer3d.org
--
-- (c) 2011-2014 Peter Althainz
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- You may obtain a copy of the License at
--
--     http://www.apache.org/licenses/LICENSE-2.0
--
-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS,
-- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
-- See the License for the specific language governing permissions and
-- limitations under the License.

-- GUI/Internal/Properties.hs

-- |Properties functionality of the GUI. Implementation module with internal data structures exposed. Public API in HGamer3D.GUI.

module HGamer3D.GUI.Internal.Properties

(

 -- * Property Types
GUIElementProperty, 
GUIButtonProperty,
GUIRadioButtonProperty, 
GUICheckBoxProperty,
GUIEditTextProperty,
GUISliderProperty,
GUISpinnerProperty,

-- * Property Functions
(=:),
setP,
getP,

-- * GUI Element Properties, for all GUI elements
pText,
pDisabled,
pVisible,
pAlpha,
pTooltip,
pAlwaysOnTop,

)        

where


import GHC.Ptr
import Data.List.Split
import Control.Monad

import HGamer3D.Data
import HGamer3D.Util

import HGamer3D.GUI.Internal.Base
import HGamer3D.GUI.Internal.Widgets
import HGamer3D.Data.HG3DClass

type GUIElementProperty a b = (GUIElement a -> IO b, GUIElement a -> b -> IO ())
type GUIButtonProperty b = GUIElementProperty GEButton b
type GUIRadioButtonProperty b = GUIElementProperty GERadioButton b
type GUICheckBoxProperty b = GUIElementProperty GECheckBox b
type GUIEditTextProperty b = GUIElementProperty GEEditText b
type GUISliderProperty b = GUIElementProperty GESlider b
type GUISpinnerProperty b = GUIElementProperty GESpinner b

(=:) :: (GUIElementProperty a b) -> b -> (GUIElement a -> IO ())
(=:) prop val = (\val' guiel -> (snd prop) guiel val') val  

setP :: GUIElement a -> [GUIElement a -> IO ()] -> IO [()]
setP guiel ps = sequence $ fmap (\f -> f guiel) ps

getP :: GUIElement a -> GUIElementProperty a b -> IO b
getP guiel p = (fst p) guiel

_stringProp :: String -> GUIElementProperty a String
_stringProp name = ( 
  (flip getGuiElProperty) name, 
  (\pname guiel val -> setGuiElProperty guiel pname val) name
  )

_toBool :: String -> Bool
_toBool instr = case instr of
  "True" -> True
  "False" -> False
  _ -> False

_fromBool :: Bool -> String
_fromBool inb = case inb of
  True -> "True"
  False -> "False"
  _ -> "False"

_toFloat :: String -> Float
_toFloat = read

_fromFloat :: Float -> String
_fromFloat = show

_boolProp :: String -> GUIElementProperty a Bool
_boolProp name = ( 
  ((flip getGuiElProperty) name) >=> return . _toBool, 
  ((\pname guiel val -> setGuiElProperty guiel pname (_fromBool val)) name)
  )

_floatProp :: String -> GUIElementProperty a Float
_floatProp name = ( 
  ((flip getGuiElProperty) name) >=> return . _toFloat, 
  ((\pname guiel val -> setGuiElProperty guiel pname (_fromFloat val)) name)
  )

pText :: GUIElementProperty a String
pText = _stringProp "Text"

pDisabled :: GUIElementProperty a Bool
pDisabled = _boolProp "Disabled"

pVisible :: GUIElementProperty a Bool
pVisible = _boolProp "Visible"

pAlpha :: GUIElementProperty a Float
pAlpha = _floatProp "Alpha"

pAlwaysOnTop :: GUIElementProperty a Bool
pAlwaysOnTop = _boolProp "AlwaysOnTop"

pTooltip :: GUIElementProperty a String
pTooltip = _stringProp "Tooltip"

