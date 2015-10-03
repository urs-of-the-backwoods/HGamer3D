-- This source file is part of HGamer3D
-- (A project to enable 3D game development in Haskell)
-- For the latest info, see http://www.hgamer3d.org
--
-- (c) 2011-2015 Peter Althainz
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

-- | the Graphics3D System of the Entity-Component-System World

{-# LANGUAGE StandaloneDeriving, OverloadedStrings, ForeignFunctionInterface #-}
{-# OPTIONS_HADDOCK hide #-}

module HGamer3D.Graphics3D.SystemGraphics3D

( 
  forkGraphics3DWorld
)

where

import Control.Monad
import Control.Concurrent.MVar
import Data.Maybe
import Data.Typeable
import Data.Dynamic
import Data.IORef
import Data.Map
import qualified Data.ByteString as B

import qualified Data.Text as T
import Data.Word

import Foreign
import Foreign.C
import Foreign.Ptr

import HGamer3D.Data
import HGamer3D.ECS
import HGamer3D.Util

import qualified HGamer3D.Binding as B
import HGamer3D.Graphics3D.Graphics3DConfig
import HGamer3D.Graphics3D.Graphics3DCommand
import HGamer3D.Graphics3D.Light
import HGamer3D.Graphics3D.Window
import HGamer3D.Graphics3D.Camera
import HGamer3D.Graphics3D.Geometry
import HGamer3D.Graphics3D.Material
import HGamer3D.Input
import HGamer3D.GUI
import HGamer3D.Audio


w64 (ComponentType w) = w

systemData = do
  lock <- newMVar ()
  ne <- newIORef []
  de <- newIORef []
  
  return $ SystemData lock ne de [] 
  
-- CH4-5s
    -- TABLE OF RECOGNIZED COMPONENTS
    
    -- Components, which are objects that means they can be created (and modified if supported): 
    [
        w64 ctGraphics3DConfig
        , w64 ctCamera
        , w64 ctLight
        , w64 ctGeometry
        
        , w64 ctMouse
        , w64 ctInputEventHandler
        
        , w64 ctButton
        , w64 ctEditText
        , w64 ctText
        , w64 ctSlider
        , w64 ctCheckBox
        , w64 ctDropDownList
        
        , w64 ctSoundSource
        , w64 ctSoundListener
        , w64 ctVolume
    ]
    
    -- Components, which are properties, they can be set on objects:
    [
        w64 ctPosition
        , w64 ctScale
        , w64 ctOrientation
        , w64 ctGraphics3DCommand
        , w64 ctMaterial
        , w64 ctColour
        , w64 ctVisible
        , w64 ctScreenRect
        , w64 ctPlayCmd
    ]
    
    -- Event receiver, they cannot set by API, they are set by C library (they send messages):
    [
        -- events are only receiver
        w64 ctMouseEvent
        , w64 ctKeyEvent
       
        -- gui elements are both sender and receiver !
        , w64 ctEditText
        , w64 ctButton
        , w64 ctSlider
        , w64 ctCheckBox
        , w64 ctDropDownList
    ]
    
-- CH4-5e                    

    B.createItem
    B.destroyItem
    B.getMessageSender
    B.registerMessageReceiver
    B.errorMessage
    
-- | start a graphics world
forkGraphics3DWorld :: IO Bool -> GameTime -> IO [SystemData] 
forkGraphics3DWorld stepF sleepT = do
                    sd <- systemData
                    runSystem stepF sleepT sd
                    return $ [sd]
 
