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

import Data.IORef
import Data.Hashable
import qualified Data.HashTable.IO as HT
import Data.List.Split


import qualified HGamer3D.Data as D
import qualified HGamer3D.Internal.Graphics3D as Gr
import qualified HGamer3D.Engine.BaseAPI as E
import qualified HGamer3D.Engine.Internal.Event as Evt

import qualified HGamer3D.GUI.BaseAPI as GU
import qualified HGamer3D.WinEvent.BaseAPI as WinEvt

import HGamer3D.Graphics3D.Schema.Figure
import HGamer3D.Graphics3D.Schema.Geometry
import HGamer3D.Graphics3D.Schema.Material
import HGamer3D.Graphics3D.Schema.Camera
import HGamer3D.Graphics3D.Schema.Light
import HGamer3D.Graphics3D.Schema.Scene
import HGamer3D.Engine.Schema.EventReceiver
import HGamer3D.GUI.Schema.Form



-- the system of entity component system, in general a system has internal state
-- entities can be added to it and the system has a step function, to run it
-- Systems are self-contained, so they can be run in a thread and manage their state themselves


data ECSGraphics3D = ECSGraphics3D {
      -- status of graphics engine
      g3d :: (Gr.Graphics3DSystem, GU.GUISystem),
      receivers :: ListAndCache EventReceiver (),

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
      orilig :: ListAndCache D.Orientation (),

      -- gui forms
      guiforms :: ListAndCache Form GU.GUIEngineData,

      -- scene parameter
      scenepars :: ListAndCache SceneParameter (),

      -- gametime
      gt :: IORef D.GameTime
      }

instance System ECSGraphics3D where

    addEntity ecsg3d entity = do
      lacAdd entity (receivers ecsg3d)
      
      lacAdd entity (figures ecsg3d)
      lacAdd entity (posfig ecsg3d)
      lacAdd entity (orifig ecsg3d)
      
      lacAdd entity (cameras ecsg3d)
      lacAdd entity (poscam ecsg3d)
      lacAdd entity (oricam ecsg3d)

      lacAdd entity (lights ecsg3d)
      lacAdd entity (poslig ecsg3d)
      lacAdd entity (orilig ecsg3d)

      lacAdd entity (guiforms ecsg3d)
      lacAdd entity (scenepars ecsg3d)
      
      return ecsg3d

    removeEntity ecsg3d entity = do
      lacRemove entity (receivers ecsg3d)
      
      lacRemove entity (figures ecsg3d)
      lacRemove entity (posfig ecsg3d)
      lacRemove entity (orifig ecsg3d)
      
      lacRemove entity (cameras ecsg3d)
      lacRemove entity (poscam ecsg3d)
      lacRemove entity (oricam ecsg3d)
      
      lacRemove entity (lights ecsg3d)
      lacRemove entity (poslig ecsg3d)
      lacRemove entity (orilig ecsg3d)
      
      lacRemove entity (guiforms ecsg3d)
      lacRemove entity (scenepars ecsg3d)
      return ecsg3d
    
    initializeSystem = do
      g3d <- E.initHGamer3D "HGamer3D" False True True
      let (g3ds, guis, gtime) = g3d
      Gr.setAmbientLight g3ds D.white

      recv <- lacInitialize CTEvR
      
      figs <- lacInitialize CTFig
      posfig <- lacInitialize CTPos
      orifig <- lacInitialize CTOri

      cams <- lacInitialize CTCam
      poscam <- lacInitialize CTPos
      oricam <- lacInitialize CTOri
        
      ligs <- lacInitialize CTLig
      poslig <- lacInitialize CTPos
      orilig <- lacInitialize CTOri

      gfos <- lacInitialize CTGFo
      scpars <- lacInitialize CTScP

      mgt <- newIORef gtime

      return $ (ECSGraphics3D (g3ds, guis) recv figs posfig orifig cams poscam oricam ligs poslig orilig gfos scpars mgt)

    stepSystem ecsg3d = do
      let (g3ds, guis) = (g3d ecsg3d)

      -- small helper functions
      let update' pos edata schema = D.positionTo edata pos 
      let update'' pos edata schema = D.orientationTo edata pos
          
      -- receivers
      lacApplyChanges (receivers ecsg3d) (\s -> return ()) (\s e -> return ()) (\e -> return ()) lacHandleU2CEvents lacHandleC2UEvents
          
      -- figures
      lacApplyChanges (figures ecsg3d) (Gr.object3D g3ds) (Gr.update3D g3ds) (Gr.remove3D g3ds) lacHandleU2CEvents lacHandleC2UEvents
      lacApplyOtherChanges (posfig ecsg3d) (figures ecsg3d) update'
      lacApplyOtherChanges (orifig ecsg3d) (figures ecsg3d) update''

      -- cameras
      let handleU2CEvents evts cam = do
            mapM (\evt -> case evt of
                     (E.WindowEvt (WinEvt.EvtWindow _ _ WinEvt.SDL_WINDOWEVENT_SIZE_CHANGED x y)) -> Gr.cameraAdaptAspectRatio cam
                     _ -> return ()
                 ) evts
            return ()
      lacApplyChanges (cameras ecsg3d) (Gr.addCamera g3ds) (Gr.updateCamera g3ds) (Gr.removeCamera g3ds) handleU2CEvents lacHandleC2UEvents
      lacApplyOtherChanges (poscam ecsg3d) (cameras ecsg3d) update'
      lacApplyOtherChanges (oricam ecsg3d) (cameras ecsg3d) update''
      
      -- lights
      lacApplyChanges (lights ecsg3d) (Gr.addLight g3ds) (Gr.updateLight g3ds) (Gr.removeLight g3ds) lacHandleU2CEvents lacHandleC2UEvents
      lacApplyOtherChanges (poslig ecsg3d) (lights ecsg3d) update'
      lacApplyOtherChanges (orilig ecsg3d) (lights ecsg3d) update''

      -- gui forms
      newFormEvents <- newIORef []
      let handleU2CEvents evts form = do
            mapM (\evt -> case evt of
                     (E.FormEvt (E.FormSetValue values)) -> GU.setFormValues form values
                     (E.GUIEvt (GU.GUIEvent tag sender window)) -> do
                       values <- GU.getFormValues form
                       strType <- GU.typeOfGuiEl window >>= return . last . (splitOn "/")
                       if tag `elem` (map fst values) then do
                         -- check if button
                         let newEvt = if strType == "Button"
                                        then E.FormEvt (E.FormButtonClick tag)
                                        else E.FormEvt (E.FormValueChange tag values)
                         -- show for testing
                         modifyIORef newFormEvents (\oldList -> (newEvt : oldList))
                         return ()
                         else return ()
                     _ -> return ()
                 ) evts
            return ()
      lacApplyChanges (guiforms ecsg3d) (GU.createForm guis) (GU.updateForm guis) (GU.removeForm guis) handleU2CEvents lacHandleC2UEvents
      
      -- scene parameters
      let updateScene eng new = Gr.setSceneParameter g3ds new
      let removeScene eng = return ()
      lacApplyChanges (scenepars ecsg3d) (Gr.setSceneParameter g3ds) updateScene removeScene lacHandleU2CEvents lacHandleC2UEvents
      
      -- step graphics system
      t <- readIORef (gt ecsg3d)
      (evts, nt, qFlag) <- E.stepHGamer3D g3ds guis t
      writeIORef (gt ecsg3d) nt

      -- handover evts towards the event system
      inList <- readIORef (lacList (receivers ecsg3d))
      evts' <- readIORef newFormEvents
      mapM (\(eid, com) -> do
               _pushC2UEvents com evts
               _pushC2UEvents com evts'
	       if qFlag then
	       	  _pushC2UEvents com [Evt.AppEvt Evt.AppQuit]
		  else return ()
           ) inList

      
      -- send camera resize events, set gui size
      let camEvts = filter (\evt -> case evt of
                                            (E.WindowEvt (WinEvt.EvtWindow _ _ WinEvt.SDL_WINDOWEVENT_SIZE_CHANGED x y)) -> True
                                            _ -> False) evts
      if length camEvts > 0 then do
        camList <- readIORef (lacList (cameras ecsg3d))
        mapM (\cam -> _pushU2CEvents (snd cam) camEvts) camList
        mapM (\(E.WindowEvt (WinEvt.EvtWindow _ _ WinEvt.SDL_WINDOWEVENT_SIZE_CHANGED x y)) -> GU.notifyDisplaySizeChanged guis (fromIntegral x) (fromIntegral y)) camEvts
        else return [()]

      -- handle GUI events, create high level gui events, push GUI events to gfos
      let guiEvts = filterEventType [GUIEvents] evts
      gfoList <- readIORef (lacList (guiforms ecsg3d))
      mapM (\gfo -> _pushU2CEvents (snd gfo) guiEvts) gfoList

      
      return (ecsg3d, qFlag)
     

    shutdownSystem ecsg3d = do
      let (g3ds, guis) = (g3d ecsg3d)
      E.freeHGamer3D g3ds guis
      return ()

runSystemGraphics3D :: D.GameTime -> IO ECSGraphics3D
runSystemGraphics3D sleepT = runSystem sleepT


