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

-- | the Graphics3D System of the Entity-Component-System World

module HGamer3D.Graphics3D.SystemGraphics3D

where

import Control.Monad
import Control.Concurrent.MVar
import qualified Data.Map as M
import Data.Maybe
import Data.Typeable
import Data.Dynamic
import Data.IORef
import Data.List.Split

import HGamer3D.Data
import HGamer3D.Common
import qualified HGamer3D.BaseAPI.Graphics3D as Gr
import HGamer3D.Graphics3D.Graphics3DSchema
import HGamer3D.Graphics3D.GUISchema

data EventReceiver = EventReceiver ERef [Gr.EventType]

data ECSGraphics3D = ECSGraphics3D Gr.Graphics3DSystem Gr.GUISystem (IORef GameTime) (IORef [EventReceiver]) (IORef Bool)

createRecordFig g3ds eref rep e ct = do
  if e #? ct then do
     l <- componentListener eref ct
     ref <- newIORef rep
     let uf = case ct of
                    CTFig -> \ _ e' -> do
                          r <- readIORef ref
                          r' <- (Gr.update3D g3ds) r (e' # CTFig)
                          when (e' #? CTPos) $ positionTo r' (e' # CTPos)
                          when (e' #? CTOri) $ orientationTo r' (e' # CTOri)
                          writeIORef ref r'
                          return () 
                    CTOri -> \ _ e' -> do
                          r <- readIORef ref
                          orientationTo r (e' # CTOri)
                          return () 
                    CTPos -> \ _ e' -> do
                          r <- readIORef ref
                          positionTo r (e' # CTPos)
                          return ()
                    _ -> \ _ _ -> return ()

     let df = return ()
     return $ Just (l, uf, df)
     else return Nothing

createRecordCam g3ds eref rep e ct = do
  if e #? ct then do
     l <- componentListener eref ct
     ref <- newIORef rep
     let uf = case ct of
                    CTCam -> \ _ e' -> do
                          r <- readIORef ref
                          r' <- (Gr.updateCamera g3ds) r (e' # CTCam)
                          when (e' #? CTPos) $ positionTo r' (e' # CTPos)
                          when (e' #? CTOri) $ orientationTo r' (e' # CTOri)
                          writeIORef ref r'
                          return () 
                    CTOri -> \ _ e' -> do
                          r <- readIORef ref
                          orientationTo r (e' # CTOri)
                          return () 
                    CTPos -> \ _ e' -> do
                          r <- readIORef ref
                          positionTo r (e' # CTPos)
                          return ()
                    _ -> \ _ _ -> return ()

     let df = return ()
     return $ Just (l, uf, df)
     else return Nothing

createRecordLight g3ds eref rep e ct = do
  if e #? ct then do
     l <- componentListener eref ct
     ref <- newIORef rep
     let uf = case ct of
                    CTLig -> \ _ e' -> do
                          r <- readIORef ref
                          r' <- (Gr.updateLight g3ds) r (e' # CTLig)
                          when (e' #? CTPos) $ positionTo r' (e' # CTPos)
                          when (e' #? CTOri) $ orientationTo r' (e' # CTOri)
                          writeIORef ref r'
                          return () 
                    CTOri -> \ _ e' -> do
                          r <- readIORef ref
                          orientationTo r (e' # CTOri)
                          return () 
                    CTPos -> \ _ e' -> do
                          r <- readIORef ref
                          positionTo r (e' # CTPos)
                          return ()
                    _ -> \ _ _ -> return ()

     let df = return ()
     return $ Just (l, uf, df)
     else return Nothing

createRecordForm guis eref rep e ct = do
  if e #? ct then do
     l <- componentListener eref ct
     ref <- newIORef rep
     let uf = case ct of
                    CTGFo -> \ _ e' -> do
                          r <- readIORef ref
                          r' <- (Gr.updateForm guis) r (e' # CTGFo)
                          writeIORef ref r'
                          return () 
                    CTCmd -> \ _ e' -> do
                          r <- readIORef ref
                          case (e' ?# CTCmd) of
                            Just (Gr.FormSetValue values) -> Gr.setFormValues r values
                            _ -> return ()
                    CTEvt -> \ _ e' -> case (e' ?# CTEvt) of
                                                   Just evts -> mapM (\evt -> do
                                                                case evt of
                                                                     Gr.GUIEvt (Gr.GUIEvent tag sender window) -> do
                                                                               r <- readIORef ref
                                                                               values <- Gr.getFormValues r
                                                                               if tag `elem` (map fst values) then do
                                                                                  -- check if button
                                                                                  strType <- Gr.typeOfGuiEl window >>= return . last . (splitOn "/")
                                                                                  let newEvt = if strType == "Button"
                                                                                               then Gr.FormEvt (Gr.FormButtonClick tag)
                                                                                               else Gr.FormEvt (Gr.FormValueChange tag values)
                                                                                  sendEvt eref [newEvt]
                                                                                  else return ()
                                                                     _ -> return () ) evts >> return ()
                                                   _ -> return ()  
                    _ -> \ _ _ -> return ()

     let df = return ()
     return $ Just (l, uf, df)
     else return Nothing

createRecordScene g3ds eref e qRef ct = do
  if e #? ct then do
     l <- componentListener eref ct
     let uf = case ct of
                    CTScP -> \ _ e' -> do
                          (Gr.setSceneParameter g3ds) (e' # CTScP)
                          return () 
                    CTCmd -> \ _ e' -> do
                          case (e' ?# CTCmd) of
                               Just Gr.AppQuit -> writeIORef qRef True
                               _ -> return ()
                          return ()
                    _ -> \ _ _ -> return ()

     let df = return ()
     return $ Just (l, uf, df)
     else return Nothing

addEvents eref system types = do
     let ECSGraphics3D g3ds guis refT refER _ = system
     erList <- readIORef refER
     let erList' = ( (EventReceiver eref types) : erList )
     writeIORef refER erList'
     return ()

instance System ECSGraphics3D where 

    initializeSystem = do

      g3d <- Gr.initHGamer3D "HGamer3D" False True True
      let (g3ds, guis, gtime) = g3d
      Gr.setAmbientLight g3ds white

      lock <- newMVar ()
      newERefs <- newIORef []
      delERefs <- newIORef []
      let records = []
      refT <- newIORef gtime
      refER <- newIORef []
      fRef <- newIORef False
      let system = ECSGraphics3D g3ds guis refT refER fRef

      let systemFunction system eref = do

          let r = []
          e <- readE eref -- this e is used to create the representation

          -- figures

          figs <- if e #? CTFig then do

             -- create figure
             rep <- Gr.object3D g3ds ((e # CTFig) :: Figure)
             if e #? CTPos then positionTo rep (e # CTPos) else return ()
             if e #? CTOri then orientationTo rep (e # CTOri) else return ()

             newRecords <- Prelude.mapM (createRecordFig g3ds eref rep e) [CTFig, CTOri, CTPos]
             return (map fromJust (filter isJust newRecords))

             else return []


          -- cameras

          cams <- if e #? CTCam then do

             -- create camera
             rep <- Gr.addCamera g3ds ((e # CTCam) :: Camera)
             if e #? CTPos then positionTo rep (e # CTPos) else return ()
             if e #? CTOri then orientationTo rep (e # CTOri) else return ()

             newRecords <- Prelude.mapM (createRecordCam g3ds eref rep e) [CTCam, CTOri, CTPos]
             return (map fromJust (filter isJust newRecords))

             else return []

          -- lights

          lights <- if e #? CTLig then do

             -- create camera
             rep <- Gr.addLight g3ds ((e # CTLig) :: Light)
             if e #? CTPos then positionTo rep (e # CTPos) else return ()
             if e #? CTOri then orientationTo rep (e # CTOri) else return ()

             newRecords <- Prelude.mapM (createRecordLight g3ds eref rep e) [CTLig, CTOri, CTPos]
             return (map fromJust (filter isJust newRecords))

             else return []

          -- guiforms

          guiforms <- if e #? CTGFo then do
             addEvents eref system [Gr.GUIEvents, Gr.FormEvents]
             rep <- Gr.createForm guis ((e # CTGFo) :: Form)
             newRecords <- Prelude.mapM (createRecordForm guis eref rep e) [CTGFo, CTOri, CTPos, CTEvt, CTCmd]
             return (map fromJust (filter isJust newRecords))
             else return []

          -- window, just receive events

          if e #? CTWin then do
             addEvents eref system [Gr.WinEvents]
             else return ()

          -- scene

          scene <- if e #? CTScP then do
             let (ECSGraphics3D g3ds guis refT refER quitRef) = system
             addEvents eref system [Gr.ApplicationEvents]
             Gr.setSceneParameter g3ds ((e # CTScP) :: SceneParameter)
             newRecords <- Prelude.mapM (createRecordScene g3ds eref e quitRef) [CTScP, CTCmd]
             return (map fromJust (filter isJust newRecords))
             else return []

          return (figs ++ cams ++ lights ++ guiforms ++ scene)

      return (SystemData lock newERefs delERefs records system systemFunction)


    stepSystem (SystemData lock newERefs delERefs records system systemFunction) = do
      let (ECSGraphics3D g3ds guis refT refER quitRef) = system
      t <- readIORef refT
      (evts, nt, qFlag) <- Gr.stepHGamer3D g3ds guis t
      let evts' = if qFlag then (Gr.AppEvt Gr.AppQuit : evts) else evts
      writeIORef refT nt
      erList <- readIORef refER
      mapM (\(EventReceiver eref evttypes) -> do
                                 let outlist = Gr.filterEventType evttypes evts'
                                 if length outlist > 0 then
                                           sendEvt eref outlist
                                           else return ()
                                           ) erList
      qFlag' <- readIORef quitRef                                     
      return qFlag'

forkGraphics3DWorld :: GameTime -> IO [SomeSystem] 
forkGraphics3DWorld sleepT = do
                    system <- (runSystem sleepT) :: IO (SystemData ECSGraphics3D)
                    return $ [SomeSystem system]

regQuitHandler envE = do
  qvar <- newEmptyMVar
  regEvtH envE (\listAppEvents -> do
                                        mapM (\appEvent -> case appEvent of
                                                                   (Gr.AppEvt Gr.AppQuit) -> sendCmd envE Gr.AppQuit >> putMVar qvar ()
                                                                   _ -> return ()) listAppEvents
                                        return ())
  return qvar

             
                 

{-

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


-}


