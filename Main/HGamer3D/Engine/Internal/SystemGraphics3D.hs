{-# Language StandaloneDeriving #-}
{-# OPTIONS_HADDOCK hide #-}

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

-- HGamer3D/Internal/ECS/SystemGraphics3D.hs

-- | the Graphics3D System of the Entity-Component-System World

module HGamer3D.Engine.Internal.SystemGraphics3D

where

import Control.Concurrent.MVar
import qualified Data.Map as M
import Data.Maybe
import Data.Typeable
import Data.Dynamic

import HGamer3D.Engine.Internal.Entity
import HGamer3D.Engine.Internal.Component
import HGamer3D.Engine.Internal.ComponentType
import HGamer3D.Engine.Internal.System

import Data.Hashable
import qualified Data.HashTable.IO as HT


import qualified HGamer3D.Data as D
import qualified HGamer3D.Internal.Graphics3D as Gr
import qualified HGamer3D.Engine.BaseAPI as E
import qualified HGamer3D.GUI.BaseAPI as GU

import HGamer3D.Graphics3D.Schema.Figure
import HGamer3D.Graphics3D.Schema.Geometry
import HGamer3D.Graphics3D.Schema.Material
import HGamer3D.Graphics3D.Schema.Camera
import HGamer3D.Graphics3D.Schema.Light



-- the system of entity component system, in general a system has internal state
-- entities can be added to it and the system has a step function, to run it
-- Systems are self-contained, so they can be run in a thread and manage their state themselves


data ECSGraphics3D = ECSGraphics3D {
      -- status of graphics engine
      g3d :: (Gr.Graphics3DSystem, GU.GUISystem),

      -- figures
      figures :: ListAndCache Figure (Gr.Object3D Figure),
      posfig :: ListAndCache D.Position (),
      orifig :: ListAndCache D.Orientation (),

      -- cameras
      cameras :: ListAndCache Camera Gr.Camera,
      poscam :: ListAndCache D.Position (),
      oricam :: ListAndCache D.Orientation (),

      -- lights
      lights :: ListAndCache Light Gr.Light,
      poslig :: ListAndCache D.Position (),
      orilig :: ListAndCache D.Orientation ()
      }

instance System ECSGraphics3D where

    addEntity ecsg3d entity = do
      lacAdd entity (figures ecsg3d)
      lacAdd entity (posfig ecsg3d)
      lacAdd entity (orifig ecsg3d)
      
      lacAdd entity (cameras ecsg3d)
      lacAdd entity (poscam ecsg3d)
      lacAdd entity (oricam ecsg3d)

      lacAdd entity (lights ecsg3d)
      lacAdd entity (poslig ecsg3d)
      lacAdd entity (orilig ecsg3d)

      return ecsg3d

    removeEntity ecsg3d entity = do
      lacRemove entity (figures ecsg3d)
      lacRemove entity (posfig ecsg3d)
      lacRemove entity (orifig ecsg3d)
      
      lacRemove entity (cameras ecsg3d)
      lacRemove entity (poscam ecsg3d)
      lacRemove entity (oricam ecsg3d)
      
      lacRemove entity (lights ecsg3d)
      lacRemove entity (poslig ecsg3d)
      lacRemove entity (orilig ecsg3d)
      
      return ecsg3d
    
    initializeSystem = do
      g3d <- E.initHGamer3D "HGamer3D - System" True True True
      let (g3ds, guis) = g3d
      Gr.setAmbientLight g3ds D.white

      figs <- lacInitialize CTFig
      posfig <- lacInitialize CTPos
      orifig <- lacInitialize CTOri

      cams <- lacInitialize CTCam
      poscam <- lacInitialize CTPos
      oricam <- lacInitialize CTOri
        
      ligs <- lacInitialize CTLig
      poslig <- lacInitialize CTPos
      orilig <- lacInitialize CTOri
        
      return $ (ECSGraphics3D g3d figs posfig orifig cams poscam oricam ligs poslig orilig)

    stepSystem ecsg3d = do
      let (g3ds, guis) = (g3d ecsg3d)
          
      -- figures
      lacApplyChanges (figures ecsg3d) (Gr.object3D g3ds) (Gr.update3D g3ds) (Gr.remove3D g3ds)
      let update' pos edata schema = D.positionTo edata pos 
      lacApplyOtherChanges (posfig ecsg3d) (figures ecsg3d) update'
      let update'' pos edata schema = D.orientationTo edata pos 
      lacApplyOtherChanges (orifig ecsg3d) (figures ecsg3d) update''

      -- cameras
      lacApplyChanges (cameras ecsg3d) (Gr.addCamera g3ds) (Gr.updateCamera g3ds) (Gr.removeCamera g3ds)
      lacApplyOtherChanges (poscam ecsg3d) (cameras ecsg3d) update'
      lacApplyOtherChanges (orifig ecsg3d) (cameras ecsg3d) update''
      
      -- lights
      lacApplyChanges (lights ecsg3d) (Gr.addLight g3ds) (Gr.updateLight g3ds) (Gr.removeLight g3ds)
      lacApplyOtherChanges (poslig ecsg3d) (lights ecsg3d) update'
      lacApplyOtherChanges (orilig ecsg3d) (lights ecsg3d) update''

      -- step graphics system
      (evt, qFlag) <- E.stepHGamer3D g3ds guis
      return (ecsg3d, qFlag)
     

    shutdownSystem ecsg3d = do
      let (g3ds, guis) = (g3d ecsg3d)
      E.freeHGamer3D g3ds guis
      return ()

runSystemGraphics3D :: D.TimeMS -> IO ECSGraphics3D
runSystemGraphics3D sleepT = runSystem sleepT


