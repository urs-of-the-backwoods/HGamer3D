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

-- HGamer3D/Internal/ECS/SystemEvent.hs

-- | the Event System of the Entity-Component-System World
-- The event system is handling the event distribution and is responsible to send incoming events and distribute them to the receivers. The following scenarios explain the event distribution in detail:
-- ** From user to component
-- The event is placed in the incoming event queue of the entity by the sendEvent function. The event systems distributes them towards the target component of the same entity. Example: play audio sound, receiving component is the audio component.
-- ** From component to user
-- The component places incoming events into the component event queue, from there the event system puts them into the entity event queue, from where they can be received by the user. Example: joystick component, window event component.
-- ** Signal/Slot/Channels to facilitate entity - entity communication
-- A channel component is a simple name, the event system holds for each channel an event list and all events, which are received by the entity are distributed to all other entities, which subscribe to the same channel. The mechanism works for incoming events from users and for incoming events from components alike.
-- ** GUI events
-- GUI elements needs events in two directions, one direction, to set element value and another one, to receive changes in the element value. Therefore also the components need two different event queues, each.
-- ** Naming conventions
-- All queues holding events in the chain user to component communication are named with U2C the queues in the direction component to user have the names C2U, this holds for the queues in the components and in the entities as well as in the event system.



module HGamer3D.Engine.Internal.SystemEvent

where

import Control.Concurrent.MVar
import Data.Maybe
import Data.Typeable
import Data.Dynamic
import Data.IORef

import HGamer3D.Engine.Internal.Entity
import HGamer3D.Engine.Internal.Component
import HGamer3D.Engine.Internal.ComponentType
import HGamer3D.Engine.Internal.System
import HGamer3D.Engine.Internal.Event

import Data.Hashable
import qualified Data.HashTable.IO as HT


import qualified HGamer3D.Data as D
import qualified HGamer3D.Internal.Graphics3D as Gr
import qualified HGamer3D.Engine.BaseAPI as E
import qualified HGamer3D.GUI.BaseAPI as GU
import qualified HGamer3D.WinEvent.BaseAPI as WinEvt

import HGamer3D.Graphics3D.Schema.Figure
import HGamer3D.Graphics3D.Schema.Geometry
import HGamer3D.Graphics3D.Schema.Material
import HGamer3D.Graphics3D.Schema.Camera
import HGamer3D.Graphics3D.Schema.Light
import HGamer3D.Graphics3D.Schema.Scene
import HGamer3D.Engine.Schema.EventReceiver
import HGamer3D.Engine.Schema.EventSender

data ECSEventQueues = ECSEventQueues {
  entList :: EntityList,
  namedQueues ::  HT.BasicHashTable String [HG3DEvent]
}

instance System ECSEventQueues where

    initializeSystem = do
      el <- entInitialize
      nq <- HT.new
      return $ (ECSEventQueues el nq) 

    addEntity esys entity = do
      entAdd (entList esys) entity
      return esys

    removeEntity esys entity = do
      entRemove (entList esys) entity
      return esys
    
    stepSystem esys = do

      -- add/remove entities from other thread
      stepEntityList (entList esys)

      -- remove, to be done

      es <- readIORef (entities (entList esys))
      -- map over all events and handle single events coming from
      -- user and from receiver components, send them to targets
      mapM (\entity -> do
               -- User Events to specific components, which want them
               u2cEvts <- _popEntityU2CEvents entity
               -- audio evts
               let audioEvts = filter (\evt -> case evt of
                                          (AudioEvt _) -> True
                                          _ -> False) u2cEvts
               case (entity #? CTASl) of
                 Nothing -> return ()
                 Just com -> _pushU2CEvents com audioEvts
               -- sender channels
               case (entity #? CTEvS) of
                 Nothing -> return ()
                 Just com -> _pushU2CEvents com u2cEvts

               -- C2U events from receiver components
               case (entity #? CTEvR) of
                 Nothing -> return ()
                 Just com -> do
                   recVal <- readC com >>= return . fromStamped . fromJust
                   c2uEvts <- _popC2UEvents com
                   let filterF = case recVal of
                         WinEventReceiver -> (\evt -> case evt of
                                                 (WindowEvt _) -> True
                                                 _ -> False)
                         GUIEventReceiver -> (\evt -> case evt of
                                                 (GUIEvt _) -> True
                                                 _ -> False)
                         UserEventReceiver -> (\evt -> case evt of
                                                 (UserEvt _) -> True
                                                 _ -> False)
                         AllEventsReceiver -> (\evt -> True)
                         _ -> (\evt -> False)
                   let evts = filter filterF c2uEvts
                   _pushEntityC2UEvents entity evts
                   -- copy to sender, in case existing
                   case (entity #? CTEvS) of
                     Nothing -> return ()
                     Just com -> _pushU2CEvents com evts
                     
               -- Channel handling, sender to named channel
               case (entity #? CTEvS) of
                 Nothing -> return ()
                 Just com -> do
                   recVal <- readC com >>= return . fromStamped . fromJust
                   case recVal of
                     ChannelEventReceiver channel -> do
                       evts <- _popU2CEvents com
                       mOldEvents <- HT.lookup (namedQueues esys) channel
                       case mOldEvents of
                         Just oldEvents -> HT.insert (namedQueues esys) channel (oldEvents ++ evts)
                         
                         Nothing -> HT.insert (namedQueues esys) channel evts
                     _ -> return ()
           ) es


      -- map over all events and handle channel events towards channel
      -- receiver
      mapM (\entity -> do
               case (entity #? CTEvR) of
                 Nothing -> return ()
                 Just com -> do
                   recVal <- readC com >>= return . fromStamped . fromJust
                   case recVal of
                        ChannelEventReceiver channel -> do
                          mEvents <- HT.lookup (namedQueues esys) channel
                          case mEvents of
                            Just events -> _pushC2UEvents com events
                            Nothing -> return ()
                        _ -> return ()
           ) es

      -- clear all channels, after sending/receiving
      namedQueueList <- HT.toList (namedQueues esys)
      mapM (\(channel, events) -> do
               HT.insert (namedQueues esys) channel []
           ) namedQueueList
      
      return (esys, False)
     

    shutdownSystem esys = return ()

runSystemEvent :: D.GameTime -> IO ECSEventQueues
runSystemEvent sleepT = runSystem sleepT


