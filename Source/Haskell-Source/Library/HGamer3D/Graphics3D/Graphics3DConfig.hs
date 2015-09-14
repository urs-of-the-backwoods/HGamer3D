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

-- HGamer3D/Graphics3D/Graphics3DConfig.hs

-- | Module providing the InitGraphics3D type
module HGamer3D.Graphics3D.Graphics3DConfig

(
	    EngineConfig (..),

        LogLevel (..),
        Logging (..),
        ctLogging,

        GraphicsQuality(..),
        ctGraphicsQuality,

        Graphics3DConfig (..),
        ctGraphics3DConfig,

        standardGraphics3DConfig
)

where

import Data.MessagePack
import Data.Text

import HGamer3D.Data
import HGamer3D.ECS
import HGamer3D.Graphics3D.Window

-- ENGINE CONFIG

-- | Base Configuration or Graphics Engine
data EngineConfig = EngineConfig {
	ig3dHeadless :: Bool, -- ^ run without graphics output
	ig3dFlushGPU :: Bool, 
	ig3dThreads :: Bool, -- ^ multi threading enabled
	ig3dForceGL2 :: Bool -- ^ only GL2 mode, no GL3
}

instance ComponentClass EngineConfig where
    toObj (EngineConfig hl fg th fgl2) = ObjectArray [ObjectBool hl, ObjectBool fg, ObjectBool th, ObjectBool fgl2]
    fromObj (ObjectArray [ObjectBool hl, ObjectBool fg, ObjectBool th, ObjectBool fgl2]) = EngineConfig hl fg th fgl2

stdEngineConfig = EngineConfig False False True False

-- LOGGING CONFIG

data LogLevel = WarningLogLevel | InfoLogLevel | DebugLogLevel

data Logging = Logging {
    logLevel :: LogLevel,
    quietLogging :: Bool,  -- ^ Logging only to file, not to std output
    logFileName :: Text   
}

instance ComponentClass LogLevel where
    toObj WarningLogLevel = ObjectInt 0
    toObj InfoLogLevel = ObjectInt 1
    toObj DebugLogLevel = ObjectInt 2
    fromObj (ObjectInt 0) = WarningLogLevel
    fromObj (ObjectInt 1) = InfoLogLevel
    fromObj (ObjectInt 2) = DebugLogLevel

instance ComponentClass Logging where
    toObj (Logging l q fn) = ObjectArray [toObj l, ObjectBool q, toObj fn]
    fromObj (ObjectArray [l, (ObjectBool q), fn_o]) = Logging (fromObj l) q (fromObj fn_o)

ctLogging :: ComponentType Logging
ctLogging = ComponentType 0x86bc15156976f061

-- GRAPHICS QUALITY

-- | Graphics Quality Setting
data GraphicsQuality = GraphicsQuality {
    gqShadow :: LMH,    -- ^ Shadow Quality
    gqMaterial :: LMH,  -- ^ Material Quality
    gqTexture :: LMH,   -- ^ Texture Quality
    gqMultisample :: LMH -- ^ Multisampling Quality
}

instance ComponentClass GraphicsQuality where
    toObj (GraphicsQuality sq mq tq ms) = ObjectArray [toObj sq, toObj mq, toObj tq, toObj ms]
    fromObj (ObjectArray [sq, mq, tq, ms]) = GraphicsQuality (fromObj sq) (fromObj mq) (fromObj tq) (fromObj ms)

ctGraphicsQuality :: ComponentType GraphicsQuality
ctGraphicsQuality = ComponentType 0x7d9cff864f27c6d2

-- GRAPHICS CONFIG

-- | Initial Configuration of Graphics Engine
data Graphics3DConfig = Graphics3DConfig EngineConfig WindowG3D (Maybe Logging) (Maybe GraphicsQuality)

mbToObj mbA = case mbA of 
	Just a -> toObj a
	Nothing -> ObjectNil

objToMb o = case o of
	ObjectNil -> Nothing
	_ -> Just (fromObj o)

instance ComponentClass Graphics3DConfig where
    toObj (Graphics3DConfig ec wc mbL mbGQ) = ObjectArray [toObj ec, toObj wc, mbToObj mbL, mbToObj mbGQ]
    fromObj (ObjectArray [ec_o, wc_o, l_o, gq_o]) = Graphics3DConfig (fromObj ec_o) (fromObj wc_o) (objToMb l_o) (objToMb gq_o)

standardGraphics3DConfig = Graphics3DConfig stdEngineConfig (xyWindow 800 600) Nothing Nothing 

ctGraphics3DConfig :: ComponentType Graphics3DConfig
ctGraphics3DConfig = ComponentType 0x0884eb62b6674bff


