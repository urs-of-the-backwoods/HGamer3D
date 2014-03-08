{-# LANGUAGE Arrows #-}

-- Some useful wires for game programming
--
-- (c) 2014 Peter Althainz
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


module HGamer3D.Wire (
  
  module HGamer3D.Wire.Types,
  module HGamer3D.Wire.EntityComponentSystem,
  module HGamer3D.Wire.ECSWire,
  module HGamer3D.Wire.GUI,
  
  HGamer3D.Wire.loopHGamer3D,
  HGamer3D.Wire.initHGamer3D,
  HGamer3D.Wire.exitHGamer3D
  
  ) where

import Control.Wire as W
import Control.Wire.Unsafe.Event as U

import Prelude hiding ((.), id)

import HGamer3D
import HGamer3D.Audio
import HGamer3D.InputSystem
import HGamer3D.WinEvent hiding (initHGamer3D, loopHGamer3D, exitHGamer3D)
import HGamer3D.GUI 

import Control.Monad.Trans.Maybe

import HGamer3D.Wire.Types
import HGamer3D.Wire.EntityComponentSystem
import HGamer3D.Wire.ECSWire
import HGamer3D.Wire.GUI
  
import Data.IORef
import qualified Data.Map as M
import Data.Maybe



-- event register mechanism

-- init

initHGamer3D :: String -- ^ Window Title
                  -> Bool -- ^ Flag show config dialogue
                  -> Bool -- ^ Flag logging enabled
                  -> Bool -- ^ show Graphics Cursor
                  -> IO (WireSystem, Camera, Viewport)
initHGamer3D windowTitle fConfigDialogue fLog fGraphicsCursor = do
  (g3ds, guis, camera, viewport) <- HGamer3D.initHGamer3D windowTitle fConfigDialogue fLog fGraphicsCursor
  un <- createUniqueName "wire"
  gedm <- newIORef (M.fromList [])
  wedm <- newIORef []
  widgetListRef <- newIORef []
  let evtDistMap = (gedm, wedm) 
  let ws = WireSystem un g3ds guis evtDistMap widgetListRef
  return (ws, camera, viewport)
  
-- loop

loopHGamer3D :: WireSystem -> IO Bool
loopHGamer3D ws = do
  let g3ds = wsG3d ws
  let guis = wsGui ws       
  let (gedm, wedm) = wsEvtDistMap ws
  (mevt, qFlag) <- HGamer3D.loopHGamer3D g3ds guis
  qFlag' <- case mevt of
        Just (EventWindow sdle) -> do
          fm <- readIORef wedm
          mapM (\fwin -> fwin sdle) fm
          case sdle of
            (EvtQuit _) -> return True
            _ -> return False
        Just (EventGUI gevts) -> do
          mapM (\gevt@(GUIEvent tag _ _) -> do
                   fm <- readIORef gedm
                   let f = fromJust (M.lookup tag fm) 
                   f gevt
                   return ()) gevts
          return False
        _ -> return False
      
  return (qFlag || qFlag') 

-- exit

exitHGamer3D = HGamer3D.exitHGamer3D
  


