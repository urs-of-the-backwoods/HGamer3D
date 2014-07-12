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
import qualified HGamer3D.Graphics3D.BaseAPI as BA
import qualified HGamer3D.Engine.BaseAPI as E
import qualified HGamer3D.GUI.BaseAPI as GU

import HGamer3D.Graphics3D.Schema.Figure
import HGamer3D.Graphics3D.Schema.Geometry
import HGamer3D.Graphics3D.Schema.Material


-- the system of entity component system, in general a system has internal state
-- entities can be added to it and the system has a step function, to run it
-- Systems are self-contained, so they can be run in a thread and manage their state themselves

type IdHashTable v = HT.BasicHashTable ComponentId v

data ECSGraphics3D = ECSGraphics3D {
      g3d :: (BA.Graphics3DSystem, GU.GUISystem, BA.Camera, BA.Viewport),
      figures :: MVar [(Component, Maybe Component, Maybe Component, Maybe Component)], -- MVar [(fig, pos, ori, size)]
      figCache :: IdHashTable (
        BA.Object3D Figure,
        StampedValue Figure,
        Maybe (StampedValue D.Position),
        Maybe (StampedValue D.Orientation),
        Maybe (StampedValue D.Size)
        )
      }

instance System ECSGraphics3D where

    addEntity ecsg3d entity = do
      oldList <- takeMVar (figures ecsg3d)
      let mFig = entity #? CTFig
      let newList = case mFig of
            Just fig -> let
              (p, o, s) = (entity #? CTPos, entity #? CTOri, entity #? CTSiz)
              in ((fig, p, o, s) : oldList)
            Nothing -> oldList
      putMVar (figures ecsg3d) newList
      return ecsg3d

    removeEntity ecsg3d entity = return ecsg3d
    
{-
    removeEntity ecsg3d entity = do
      let (g3ds, guis, camera, viewport) = g3d
      let removeFunction mvList cache = do
            oldList <- takeMVar mvList
            
            let newList = filter ( (/=) 
-}

    initializeSystem = do
        g3d <- E.initHGamer3D "HGamer3D - System" True True True
        setupBasicCamera g3d
        figures <- newMVar []
        figCache <- HT.new
        return $ (ECSGraphics3D g3d figures figCache)


    stepSystem ecsg3d = do
      let (g3ds, guis, camera, viewport) = (g3d ecsg3d)
      -- update 3d objects
      cList <- takeMVar (figures ecsg3d)
      mapM (\(cf, mcp, mco, mcs) -> do
               -- empty event queues
               _popEvents cf
               -- handle changes per single entity item
               figTVal <- readC cf >>= return . fromJust
               let newFig = fromStamped figTVal
               mCache <- HT.lookup (figCache ecsg3d) (idC cf)
               -- first create/update the figure object
               newOb <- case mCache of
                 Nothing -> BA.object3D g3ds newFig
                 Just (oldOb, oldFig, mPos, mOri, mSiz) -> if oldFig /= figTVal then BA.update3D g3ds oldOb newFig else return oldOb
               -- then handle the POS information - Position
               newPos <- case mcp of
                 Just cp -> do
                   cpTVal <- readC cp >>= return . fromJust
                   let cpVal = fromStamped cpTVal
                   case mCache of
                     Nothing -> D.positionTo newOb cpVal
                     Just (oldOb, oldFig, mPos, mOri, mSiz) -> if (fromJust mPos) /= cpTVal then D.positionTo newOb cpVal else return ()
                   return $ Just cpTVal
                 Nothing -> return Nothing
               -- then handle the POS information - Orientation
               newOri <- case mco of
                 Just co -> do
                   coTVal <- readC co >>= return . fromJust
                   let coVal = fromStamped coTVal
                   case mCache of
                     Nothing -> D.orientationTo newOb coVal
                     Just (oldOb, oldFig, mPos, mOri, mSiz) -> if (fromJust mOri) /= coTVal then D.orientationTo newOb coVal else return ()
                   return $ Just coTVal
                 Nothing -> return Nothing
               -- then handle the POS information - Size
               newSiz <- case mcs of
                 Just cs -> do
                   csTVal <- readC cs >>= return . fromJust
                   let csVal = fromStamped csTVal
                   case mCache of
                     Nothing -> D.sizeTo newOb csVal
                     Just (oldOb, oldFig, mPos, mOri, mSiz) -> if (fromJust mSiz) /= csTVal then D.sizeTo newOb csVal else return ()
                   return $ Just csTVal
                 Nothing -> return Nothing
               -- insert new values into cache
               HT.insert (figCache ecsg3d) (idC cf) (newOb, figTVal, newPos, newOri, newSiz)
           ) cList
      putMVar (figures ecsg3d) cList
      -- run graphics
      (evt, qFlag) <- E.stepHGamer3D g3ds guis
      return (ecsg3d, qFlag)

    shutdownSystem ecsg3d = do
      let (g3ds, guis, camera, viewport) = (g3d ecsg3d)
      E.freeHGamer3D g3ds guis
      return ()

runSystemGraphics3D :: D.TimeMS -> IO ECSGraphics3D
runSystemGraphics3D sleepT = runSystem sleepT


setupBasicCamera g3d = do
  let (g3ds, guis, camera, viewport) = g3d
  -- camera position
  let pos = D.Vec3 5.0 5.0 80.0
  D.positionTo camera pos
  let at = D.Vec3 0.0 0.0 (-300.0)
  BA.cameraLookAt camera at
                                           
  -- define light
            
  BA.setAmbientLight g3ds D.white
  BA.pointLight g3ds D.white (D.Vec3 10.0 10.0 20.0)
        
  -- GUI Code starts here, display hg3d logo
  GU.loadGuiScheme guis "hg3d.scheme"
  logo <- GU.loadGuiLayoutFromFile guis "hgamer3d.layout" ""
  GU.addGuiElToDisplay guis logo
  
  return ()
