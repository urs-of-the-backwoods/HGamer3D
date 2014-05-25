-- This source file is part of HGamer3D
-- (A project to enable 3D game development in Haskell)
-- For the latest info, see http://www.hgamer3d.org
--
-- (c) 2011-2013 Peter Althainz
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

-- ColourValue.hs

-- | Module providing the Colour type and some standard colours
module HGamer3D.Data.Colour 

(
	Colour (Colour, cRed, cGreen, cBlue, cAlpha),
        white,
        silver,
        grey,
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

-- | HGamer3D colour type
data Colour = Colour {
  cRed :: Float,   -- ^ red component ranging from 0.0 to 1.0
  cGreen :: Float, -- ^ green component ranging from 0.0 to 1.0
  cBlue :: Float,  -- ^ blue component ranging from 0.0 to 1.0
  cAlpha :: Float  -- ^ alpha component ranging from 0.0 to 1.0 with 1.0 being fully opaque
   } deriving (Eq, Show)

white = Colour 1.0 1.0 1.0 1.0 
silver  = Colour 0.75 0.75 0.75 1.0
grey = Colour 0.5 0.5 0.5 1.0
black = Colour 0.0 0.0 0.0 1.0
red = Colour 1.0 0.0 0.0 1.0
maroon = Colour 0.5 0.0 0.0 1.0
yellow = Colour 1.0 1.0 0.0 1.0
olive = Colour 0.5 0.5 0.0 1.0
lime = Colour 0.0 1.0 0.0 1.0
green = Colour 0.0 0.5 0.0 1.0
aqua = Colour 0.0 1.0 1.0 1.0
teal = Colour 0.0 0.5 0.5 1.0
blue = Colour 0.0 0.0 1.0 1.0 
navy = Colour 0.0 0.0 0.5 1.0
fuchsia = Colour 1.0 0.0 1.0 1.0
purple = Colour 0.5 0.0 0.5 1.0



