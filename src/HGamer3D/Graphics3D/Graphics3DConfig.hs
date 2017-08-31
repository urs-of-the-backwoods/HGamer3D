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
import Data.Binary.Serialise.CBOR.Encoding

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


data LogLevel = Warning
    | Info
    | Debug
    deriving (Eq, Read, Show)


data Logging = Logging {
    loggingLogLevel::LogLevel,
    loggingQuietLogging::Bool,
    loggingLogFileName::Text
    } deriving (Eq, Read, Show)


data WindowG3D = WindowG3D {
    windowG3DWidth::Int,
    windowG3DHeight::Int,
    windowG3DBorderless::Bool,
    windowG3DFullScreen::Bool,
    windowG3DResizable::Bool
    } deriving (Eq, Read, Show)


data GraphicsQuality = GraphicsQuality {
    graphicsQualityShadow::LMH,
    graphicsQualityMaterial::LMH,
    graphicsQualityTexture::LMH,
    graphicsQualityMultisample::LMH
    } deriving (Eq, Read, Show)


data Graphics3DConfig = Graphics3DConfig {
    graphics3DConfigEngine::EngineConfig,
    graphics3DConfigQuality::GraphicsQuality,
    graphics3DConfigLogging::Logging,
    graphics3DConfigWindow::WindowG3D
    } deriving (Eq, Read, Show)


ctGraphics3DConfig :: ComponentType Graphics3DConfig
ctGraphics3DConfig = ComponentType 0x884eb62b6674bff

instance Serialise EngineConfig where
    encode (EngineConfig v1 v2 v3 v4) = encodeListLen 4 <> encode v1 <> encode v2 <> encode v3 <> encode v4
    decode = decodeListLenOf 4 >> EngineConfig <$> decode <*> decode <*> decode <*> decode

instance Serialise LogLevel where
    encode (Warning) = encodeListLen 1 <>  encode (0::Int) 
    encode (Info) = encodeListLen 1 <>  encode (1::Int) 
    encode (Debug) = encodeListLen 1 <>  encode (2::Int) 
    decode = do
        decodeListLen
        i <- decode :: Decoder s Int
        case i of
            0 -> (pure Warning)
            1 -> (pure Info)
            2 -> (pure Debug)

instance Serialise Logging where
    encode (Logging v1 v2 v3) = encodeListLen 3 <> encode v1 <> encode v2 <> encode v3
    decode = decodeListLenOf 3 >> Logging <$> decode <*> decode <*> decode

instance Serialise WindowG3D where
    encode (WindowG3D v1 v2 v3 v4 v5) = encodeListLen 5 <> encode v1 <> encode v2 <> encode v3 <> encode v4 <> encode v5
    decode = decodeListLenOf 5 >> WindowG3D <$> decode <*> decode <*> decode <*> decode <*> decode

instance Serialise GraphicsQuality where
    encode (GraphicsQuality v1 v2 v3 v4) = encodeListLen 4 <> encode v1 <> encode v2 <> encode v3 <> encode v4
    decode = decodeListLenOf 4 >> GraphicsQuality <$> decode <*> decode <*> decode <*> decode

instance Serialise Graphics3DConfig where
    encode (Graphics3DConfig v1 v2 v3 v4) = encodeListLen 4 <> encode v1 <> encode v2 <> encode v3 <> encode v4
    decode = decodeListLenOf 4 >> Graphics3DConfig <$> decode <*> decode <*> decode <*> decode




-- output sinopia ends here

standardGraphics3DConfig = Graphics3DConfig 
    (EngineConfig False False True False)
    (GraphicsQuality Medium Medium Medium Medium) 
    (Logging Debug False "hgamer3d.log") 
    (WindowG3D 800 600 False False True) 





