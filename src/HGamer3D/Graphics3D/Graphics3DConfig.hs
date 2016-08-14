{-# LANGUAGE OverloadedStrings #-}
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

import Fresco
import Data.MessagePack
import Data.Text

import HGamer3D.Data
import HGamer3D.Graphics3D.Window


-- OUTPUT SINOPIA START HERE

data EngineConfig = EngineConfig {
    engineConfigHeadless::Bool,
    engineConfigFlushGPU::Bool,
    engineConfigThreads::Bool,
    engineConfigForceGL2::Bool
}

instance ComponentClass EngineConfig where
    toObj (EngineConfig v1 v2 v3 v4) = ObjectArray [ObjectBool v1, ObjectBool v2, ObjectBool v3, ObjectBool v4]
    fromObj (ObjectArray [ObjectBool v1, ObjectBool v2, ObjectBool v3, ObjectBool v4]) = EngineConfig v1 v2 v3 v4

data LogLevel = Warning
    | Info
    | Debug
    deriving (Eq, Read, Show)

instance ComponentClass LogLevel where
    toObj (Warning) = ObjectArray [ObjectInt 0, ObjectArray []]
    toObj (Info) = ObjectArray [ObjectInt 1, ObjectArray []]
    toObj (Debug) = ObjectArray [ObjectInt 2, ObjectArray []]
    fromObj (ObjectArray [ObjectInt 0, ObjectArray []]) = Warning
    fromObj (ObjectArray [ObjectInt 1, ObjectArray []]) = Info
    fromObj (ObjectArray [ObjectInt 2, ObjectArray []]) = Debug

data Logging = Logging {
    loggingLogLevel::LogLevel,
    loggingQuietLogging::Bool,
    loggingLogFileName::Text
}

instance ComponentClass Logging where
    toObj (Logging v1 v2 v3) = ObjectArray [(toObj v1), ObjectBool v2, (toObj v3)]
    fromObj (ObjectArray [v1, ObjectBool v2, v3]) = Logging (fromObj v1) v2 (fromObj v3)

data GraphicsQuality = GraphicsQuality {
    graphicsQualityShadow::QualityLMH,
    graphicsQualityMaterial::QualityLMH,
    graphicsQualityTexture::QualityLMH,
    graphicsQualityMultisample::QualityLMH
}

instance ComponentClass GraphicsQuality where
    toObj (GraphicsQuality v1 v2 v3 v4) = ObjectArray [(toObj v1), (toObj v2), (toObj v3), (toObj v4)]
    fromObj (ObjectArray [v1, v2, v3, v4]) = GraphicsQuality (fromObj v1) (fromObj v2) (fromObj v3) (fromObj v4)

data Graphics3DConfig = Graphics3DConfig {
    graphics3DConfigEngine::EngineConfig,
    graphics3DConfigQuality::GraphicsQuality,
    graphics3DConfigLogging::Logging,
    graphics3DConfigWindow::WindowG3D
}

instance ComponentClass Graphics3DConfig where
    toObj (Graphics3DConfig v1 v2 v3 v4) = ObjectArray [(toObj v1), (toObj v2), (toObj v3), (toObj v4)]
    fromObj (ObjectArray [v1, v2, v3, v4]) = Graphics3DConfig (fromObj v1) (fromObj v2) (fromObj v3) (fromObj v4)

-- OUTPUT SINOPIA ENDS HERE

stdEngineConfig = EngineConfig False False True False

ctLogging :: ComponentType Logging
ctLogging = ComponentType 0x86bc15156976f061

ctGraphicsQuality :: ComponentType GraphicsQuality
ctGraphicsQuality = ComponentType 0x7d9cff864f27c6d2

standardGraphics3DConfig = Graphics3DConfig stdEngineConfig (GraphicsQuality Medium Medium Medium Medium) (Logging Debug False "hgamer3d.log") (xyWindow 800 600) 

ctGraphics3DConfig :: ComponentType Graphics3DConfig
ctGraphics3DConfig = ComponentType 0x0884eb62b6674bff




