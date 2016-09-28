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

-- HGamer3D/Graphics3D/Camera.hs

{-# LANGUAGE OverloadedStrings #-}

-- | Module providing the Material type
module HGamer3D.Graphics3D.Material

(
		-- * The Material Type and ComponentType
        Material (..),
        ctMaterial,

        -- * Some materials to play with
		matOrangeCrossMetal,
		matCrossMetal,
		matCrossMetalBlue,
		matMetal,
		matMetalZigZag,
		matMetalBumps,
		matFishEye,
		matMetalOrnament,
		matMetalScratch,
		matMetalLine,
		matGreenGrass,
		matBrownGrass,
		matGreyGrass,
		matSand,
		matRedRock,
		matBlackRock,
		matBrownStone,
		matStoneMetalWall,
		matCoalWall,
		matBrickWallGray,
		matBrickWallRed,
		matTilesOrange,
		matWoodTiles,
		matColourTiles,
		matBlackTiles,

		matWhite,
		matSilver,
		matGrey,
		matDarkGrey,
		matBlack,
		matRed,
		matMaroon,
		matYellow,
		matOlive,
		matLime,
		matGreen,
		matAqua,
		matTeal,
		matBlue,
		matNavy,
		matFuchsia,
		matPurple 
)

where

import Data.MessagePack
import Fresco
import Data.Text

import HGamer3D.Data

-- HGamer3D website, entities, example 1 for data types

data Material = ResourceMaterial Text
    deriving (Eq, Read, Show)

instance ComponentClass Material where
    toObj (ResourceMaterial v1) = ObjectArray [ObjectInt 0, ObjectArray [(toObj v1)]]
    fromObj (ObjectArray [ObjectInt 0, ObjectArray [v1]]) = ResourceMaterial (fromObj v1)

ctMaterial :: ComponentType Material
ctMaterial = ComponentType 0xb4bae8b0d0d8c162

-- some materials from the material folder
matOrangeCrossMetal = ResourceMaterial "Materials/Pattern_01.xml"
matCrossMetal = ResourceMaterial "Materials/Pattern_03.xml"
matCrossMetalBlue = ResourceMaterial "Materials/Pattern_04.xml"
matMetal = ResourceMaterial "Materials/Pattern_12.xml"
matMetalZigZag = ResourceMaterial "Materials/Pattern_13.xml"
matMetalBumps = ResourceMaterial "Materials/Pattern_14.xml"
matFishEye = ResourceMaterial "Materials/Pattern_15.xml"
matMetalOrnament = ResourceMaterial "Materials/Pattern_29.xml"
matMetalScratch = ResourceMaterial "Materials/Pattern_30.xml"
matMetalLine = ResourceMaterial "Materials/Pattern_31.xml"
matGreenGrass = ResourceMaterial "Materials/Pattern_42.xml"
matBrownGrass = ResourceMaterial "Materials/Pattern_43.xml"
matGreyGrass = ResourceMaterial "Materials/Pattern_44.xml"
matSand = ResourceMaterial "Materials/Pattern_45.xml"
matRedRock = ResourceMaterial "Materials/Pattern_46.xml"
matBlackRock = ResourceMaterial "Materials/Pattern_124.xml"
matBrownStone = ResourceMaterial "Materials/Pattern_125.xml"
matStoneMetalWall = ResourceMaterial "Materials/Pattern_126.xml"
matCoalWall = ResourceMaterial "Materials/Pattern_127.xml"
matBrickWallGray = ResourceMaterial "Materials/Pattern_100.xml"
matBrickWallRed = ResourceMaterial "Materials/Pattern_102.xml"
matTilesOrange = ResourceMaterial "Materials/Pattern_270.xml"
matWoodTiles = ResourceMaterial "Materials/Pattern_271.xml"
matColourTiles = ResourceMaterial "Materials/Pattern_272.xml"
matBlackTiles = ResourceMaterial "Materials/Pattern_273.xml"

matWhite = ResourceMaterial "Materials/ColourWhite.xml"
matSilver  = ResourceMaterial "Materials/ColourSilver.xml"
matGrey = ResourceMaterial "Materials/ColourGrey.xml" 
matDarkGrey = ResourceMaterial "Materials/ColourDarkGrey.xml"
matBlack = ResourceMaterial "Materials/ColourBlack.xml" 
matRed = ResourceMaterial "Materials/ColourRed.xml"
matMaroon = ResourceMaterial "Materials/ColourMaroon.xml"
matYellow = ResourceMaterial "Materials/ColourYellow.xml"
matOlive = ResourceMaterial "Materials/ColourOlive.xml" 
matLime = ResourceMaterial "Materials/ColourLime.xml" 
matGreen = ResourceMaterial "Materials/ColourGreen.xml"
matAqua = ResourceMaterial "Materials/ColourAqua.xml" 
matTeal = ResourceMaterial "Materials/ColourTeal.xml" 
matBlue = ResourceMaterial "Materials/ColourBlue.xml"  
matNavy = ResourceMaterial "Materials/ColourNavy.xml"
matFuchsia = ResourceMaterial "Materials/ColourFuchsia.xml"
matPurple = ResourceMaterial "Materials/ColourPurple.xml"

-- end of website text


