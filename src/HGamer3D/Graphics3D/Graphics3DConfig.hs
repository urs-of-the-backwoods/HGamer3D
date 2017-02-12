-- This source file is part of HGamer3D
-- (A project to enable 3D game development in Haskell)
-- For the latest info, see http://www.hgamer3d.org
--
-- (c) 2011 - 2017 Peter Althainz
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

-- file: HGamer3D/Graphics3D/Graphics3DConfig.hs

{-# LANGUAGE OverloadedStrings #-}

module HGamer3D.Graphics3D.Graphics3DConfig
where

-- output sinopia starts here

import Fresco
import Data.Binary.Serialise.CBOR
import Data.Binary.Serialise.CBOR.Decoding

import Data.Text
import Data.Monoid
import Control.Applicative

import HGamer3D.Data.LMH


data EngineConfig = EngineConfig {
    engineConfigHeadless::Bool,
    engineConfigFlushGPU::Bool,
    engineConfigThreads::Bool,
    engineConfigForceGL2::Bool
    } deriving (Eq, Read, Show)


instance Serialise EngineConfig where
    encode (EngineConfig v1 v2 v3 v4) = encode v1 <> encode v2 <> encode v3 <> encode v4
    decode = EngineConfig <$> decode <*> decode <*> decode <*> decode

data LogLevel = Warning
    | Info
    | Debug
    deriving (Eq, Read, Show)


instance Serialise LogLevel where
    encode (Warning) = encode (0::Int) 
    encode (Info) = encode (1::Int) 
    encode (Debug) = encode (2::Int) 
    decode = do
        i <- decode :: Decoder Int
        case i of
            0 -> (pure Warning)
            1 -> (pure Info)
            2 -> (pure Debug)

data Logging = Logging {
    loggingLogLevel::LogLevel,
    loggingQuietLogging::Bool,
    loggingLogFileName::Text
    } deriving (Eq, Read, Show)


instance Serialise Logging where
    encode (Logging v1 v2 v3) = encode v1 <> encode v2 <> encode v3
    decode = Logging <$> decode <*> decode <*> decode

data WindowG3D = WindowG3D {
    windowG3DWidth::Int,
    windowG3DHeight::Int,
    windowG3DBorderless::Bool,
    windowG3DFullScreen::Bool,
    windowG3DResizable::Bool
    } deriving (Eq, Read, Show)


instance Serialise WindowG3D where
    encode (WindowG3D v1 v2 v3 v4 v5) = encode v1 <> encode v2 <> encode v3 <> encode v4 <> encode v5
    decode = WindowG3D <$> decode <*> decode <*> decode <*> decode <*> decode

data GraphicsQuality = GraphicsQuality {
    graphicsQualityShadow::LMH,
    graphicsQualityMaterial::LMH,
    graphicsQualityTexture::LMH,
    graphicsQualityMultisample::LMH
    } deriving (Eq, Read, Show)


instance Serialise GraphicsQuality where
    encode (GraphicsQuality v1 v2 v3 v4) = encode v1 <> encode v2 <> encode v3 <> encode v4
    decode = GraphicsQuality <$> decode <*> decode <*> decode <*> decode

data Graphics3DConfig = Graphics3DConfig {
    graphics3DConfigEngine::EngineConfig,
    graphics3DConfigQuality::GraphicsQuality,
    graphics3DConfigLogging::Logging,
    graphics3DConfigWindow::WindowG3D
    } deriving (Eq, Read, Show)


instance Serialise Graphics3DConfig where
    encode (Graphics3DConfig v1 v2 v3 v4) = encode v1 <> encode v2 <> encode v3 <> encode v4
    decode = Graphics3DConfig <$> decode <*> decode <*> decode <*> decode

ctGraphics3DConfig :: ComponentType Graphics3DConfig
ctGraphics3DConfig = ComponentType 0x884eb62b6674bff

-- output sinopia ends here

standardGraphics3DConfig = Graphics3DConfig 
    (EngineConfig False False True False)
    (GraphicsQuality Medium Medium Medium Medium) 
    (Logging Debug False "hgamer3d.log") 
    (WindowG3D 800 600 False False True) 





