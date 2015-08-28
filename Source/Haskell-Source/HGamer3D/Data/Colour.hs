-- This source file is part of HGamer3D
-- (A project to enable 3D game development in Haskell)
-- For the latest info, see http://www.hgamer3d.org
--
-- (c) 2011 - 2015 Peter Althainz
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

-- HGamer3D/Data/Colour.hs

-- | The Colour type and some standard colours
module HGamer3D.Data.Colour

(
        Colour (..),
        ctColour,
        white,
        silver,
        grey,
	darkgrey,
        black,
        red,
        maroon,
        yellow,
        olive,
        lime,
        green,
        aqua,
        teal,
        blue,
        navy,
        fuchsia,
        purple
)

where

import Data.MessagePack
import HGamer3D.Data.Component

data Colour = Colour {cRed::Float, cGreen::Float, cBlue::Float, cAlpha::Float} deriving (Eq, Show)

instance ComponentClass Colour where
        toObj (Colour r g b a) = ObjectArray [ObjectFloat r, ObjectFloat g, ObjectFloat b, ObjectFloat a]
        fromObj (ObjectArray [ObjectFloat r, ObjectFloat g, ObjectFloat b, ObjectFloat a]) = (Colour r g b a)

ctColour :: ComponentType Colour
ctColour = ComponentType 0xe202add0521cde41

white :: Colour
white = Colour 1.0 1.0 1.0 1.0

silver:: Colour
silver  = Colour 0.75 0.75 0.75 1.0

grey:: Colour
grey = Colour 0.5 0.5 0.5 1.0

darkgrey :: Colour
darkgrey = Colour 0.25 0.25 0.25 1.0

black :: Colour
black = Colour 0.0 0.0 0.0 1.0

red :: Colour
red = Colour 1.0 0.0 0.0 1.0

maroon :: Colour
maroon = Colour 0.5 0.0 0.0 1.0

yellow :: Colour
yellow = Colour 1.0 1.0 0.0 1.0

olive :: Colour
olive = Colour 0.5 0.5 0.0 1.0

lime :: Colour
lime = Colour 0.0 1.0 0.0 1.0

green :: Colour
green = Colour 0.0 0.5 0.0 1.0

aqua :: Colour
aqua = Colour 0.0 1.0 1.0 1.0

teal :: Colour
teal = Colour 0.0 0.5 0.5 1.0

blue :: Colour
blue = Colour 0.0 0.0 1.0 1.0

navy :: Colour
navy = Colour 0.0 0.0 0.5 1.0

fuchsia :: Colour
fuchsia = Colour 1.0 0.0 1.0 1.0

purple :: Colour
purple = Colour 0.5 0.0 0.5 1.0



