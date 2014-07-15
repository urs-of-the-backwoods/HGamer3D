{-# Language StandaloneDeriving, ExistentialQuantification #-}
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

-- System.hs

-- | the System in Entity-Component-System

module HGamer3D.Engine.Internal.System

where

import Control.Concurrent
import Control.Concurrent.MVar
import qualified Data.Map as M
import Data.Maybe
import Data.Typeable

import HGamer3D.Data as D
import HGamer3D.Engine.Internal.Component
import HGamer3D.Engine.Internal.Entity

import System.Clock
import System.Mem.StableName
import Data.Hashable

-- the system of entity component system, in general a system has internal state
-- entities can be added to it and the system has a step function, to run it

class System a where
      addEntity :: a -> Entity -> IO a
      removeEntity :: a -> Entity -> IO a
      stepSystem :: a -> IO (a, Bool)
      initializeSystem :: IO a
      shutdownSystem :: a -> IO ()

      runSystem :: D.TimeMS -> IO a
      runSystem stepT = do
        mv <- newEmptyMVar
        forkOS $ (\mv' -> do
                     status <- initializeSystem
                     putMVar mv' status
                     let runS s = do
                            let (TimeMS msecStepT) = stepT
                            nowT <- getTime Monotonic
                            (s', qFlag) <- stepSystem s
                            if qFlag then do
                              shutdownSystem s'
                              return ()
                              else do
                                nowT' <- getTime Monotonic
                                let diffSec = (fromIntegral ((sec nowT') - (sec nowT)))::Integer
                                let diffNSec = (fromIntegral ((nsec nowT') - (nsec nowT)))::Integer
                                let diffMSec = diffSec * 1000 + (diffNSec `div` 1000000)
                                let msecDelay = (fromIntegral msecStepT) - diffMSec
                                if msecDelay > 0 then do
                                  threadDelay ((fromIntegral msecDelay) * 1000)
                                  else do
                                    return ()
                                runS s'
                     runS status
                     ) mv
        status' <- takeMVar mv
        return status'


-- management of systems
--
        
data SomeSystem = forall a . System a => SomeSystem a

(#+) :: forall a. System a => a -> [SomeSystem] -> [SomeSystem]
a #+ as = (SomeSystem a : as) 
infixr #+

-- ECS World functions, to manage entities in systems

addToWorld :: [SomeSystem] -> Entity -> IO ()
addToWorld systems e = mapM (f e) systems >> return () where
  f e (SomeSystem s) = addEntity s e >> return ()

removeFromWorld :: [SomeSystem] -> Entity -> IO ()
removeFromWorld systems e = mapM (f e) systems >> return () where
  f e (SomeSystem s) = removeEntity s e >> return ()
    
