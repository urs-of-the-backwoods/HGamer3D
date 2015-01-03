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

{- | the Event System of the Entity-Component-System World
The event system is handling the event distribution and is responsible to send incoming events and distribute them to the receivers. The guidelines how this works are along the main ideas of an ECS in general, each component should be separated from each other. The event system is the only one, which knows about more than one component with regards to events. Also the user interface in the end should be simple. 

* User Interface Functionality

Users receive and send events over the entity interface with the functions receiveEvents and sendEvent. Sent events are distributed to the components in the entity as needed. Events coming from components are only processed if a ReceiveEvent component is added to the entity with the filter criteria, which events should be received. An additional channel component is existing, which allows sending events to a channel and receiving events from a channel.

* Scenarios, how the Event System works in detail

In general users are only interacting with the entity functions, which fill and empty the U2C and C2U queues of the entity. Component systems (with the exception of the EventSystem itself) are only receiving and sending into the component queues. The distribution of events between entities and components are done by the EventSystem, based on two principle mechanisms: 

** From user to component (U2C)
U2C events are placed into the appropriate components directly. The event is placed in the incoming event queue of the entity by the sendEvent function. The event systems distributes them towards the target component of the same entity. into the U2C queue. The target component picks it up from the U2C queue and handles it. Examples: play audio sound, receiving component is the audio component. GUI Form set values, receiving component is the GUI component (within Graphics3D), which handles it.

** From component to user (C2U)
C2U events are not automatically placed into the entity for receipt by the user. Instead all C2U events are placed into an exising receiving component. From this component only the events of the current filter criteria are put into the entity C2U event queue. Besides of that processing is similar as in the U2C case. The component places incoming events into the component event queue, the C2U queue, from there the event system puts them into the receiving component queue. 

** Channels
Channels are just separate components, which work the following: user events are put in the component channel U2C queue. From there they are stored into a cross entity channel storage structure. All the events in the channel structure are distributed to the channel components in the C2U queue. From there they might be picked up by receiving components, as done in the case of other components also.
-}

module HGamer3D.Graphics3D.SystemEvent

where

import Control.Concurrent.MVar
import Data.Maybe
import Data.Typeable
import Data.Dynamic
import Data.IORef

import HGamer3D.Engine.Internal.ECS
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
import HGamer3D.Engine.Schema.EventChannel

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

      es <- readIORef (entities (entList esys))
      
      -- map over all entities and handle the events
      mapM (\entity -> do

               -- U2C mechanism, distribute incoming user events to components which wants them
               
               u2cEvts <- _popEntityU2CEvents entity
               -- audio evts
               let audioEvts = filterEventType [AudioEvents] u2cEvts
               case (entity #? CTASl) of
                 Nothing -> return ()
                 Just com -> _pushU2CEvents com audioEvts
               -- channels
               case (entity #? CTEvC) of
                 Nothing -> return ()
                 Just com -> _pushU2CEvents com u2cEvts
               -- GUI Forms
               let guiEvts = filterEventType [FormEvents] u2cEvts
               case (entity #? CTGFo) of
                 Nothing -> return ()
                 Just com -> _pushU2CEvents com guiEvts

               -- Channel handling, send U2C events to channel
                  
               case (entity #? CTEvC) of
                 Nothing -> return ()
                 Just com -> do
                   EventChannel channel <- readC com >>= return . fromStamped . fromJust
                   evts <- _popU2CEvents com
                   mOldEvents <- HT.lookup (namedQueues esys) channel
                   case mOldEvents of
                     Just oldEvents -> HT.insert (namedQueues esys) channel (oldEvents ++ evts)
                     Nothing -> HT.insert (namedQueues esys) channel evts
                     _ -> return ()
                 
               -- C2U mechanism, distribute incoming component events to entity
                 
               case (entity #? CTEvR) of
                 Nothing -> return ()
                 Just com -> do
                   (EventReceiver eventtypes) <- readC com >>= return . fromStamped . fromJust
                   c2uEvts <- _popC2UEvents com
                   let evts = filterEventType eventtypes c2uEvts
                   _pushEntityC2UEvents entity evts
                     
           ) es

      -- remark channel handling: the correct sequence is as follows, first
      -- handle all entities and put user events into channel storage (above).
      -- Then (second) send all channels to receivers (C2U), which is below.
      -- Third, delete queues.

      -- Channel handling, send all queue content to all receivers
      -- clear all channels, after sending/receiving
      namedQueueList <- HT.toList (namedQueues esys)
      mapM (\(channel, events) -> do
               events <- HT.lookup (namedQueues esys) channel >>= return . fromJust
               HT.insert (namedQueues esys) channel []
               mapM (\entity -> do
                        case (entity #? CTEvC) of
                          Nothing -> return ()
                          Just comChannel -> do
                            EventChannel channel' <- readC comChannel >>= return . fromStamped . fromJust
                            if channel == channel' then do
                              case (entity #? CTEvR) of
                                Nothing -> return ()
                                Just comReceiver -> _pushC2UEvents comReceiver events
                              else return ()
                    ) es
           ) namedQueueList
      
      return (esys, False)
     

    shutdownSystem esys = return ()

runSystemEvent :: D.GameTime -> IO ECSEventQueues
runSystemEvent sleepT = runSystem sleepT


