-- This source file is part of HGamer3D
-- (A project to enable 3D game development in Haskell)
-- For the latest info, see http://www.althainz.de/HGamer3D.html
--
-- (c) 2015 Peter Althainz
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

-- HGamer3D/ECS/System.hs

{-# LANGUAGE ForeignFunctionInterface #-}

-- | System of the Entity Component System for HGamer3D
module HGamer3D.ECS.System
(
  SystemData (..),
  runSystem,
  addToWorld,
  removeFromWorld
)
where

import Data.Maybe
import Data.Dynamic
import Data.Typeable
import Data.List as L
import qualified Data.Map as M
import Data.IORef

import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as BU

import Foreign
import Foreign.C
import Foreign.C.String
import Foreign.Ptr

import Control.Concurrent
import Control.Applicative

import HGamer3D.Binding
import HGamer3D.Data 
import HGamer3D.ECS.Entity


-- System


-- ComponentListener, used internally in sytems

type ComponentListener = IORef (Maybe (EntityData, EntityData))

componentListener :: Entity -> ComponentType a -> IO ComponentListener
componentListener er@(Entity te tl) c  = do
           tv <- newIORef Nothing
           let w e e' = atomicModifyIORef tv (\old -> let
                                                        new = case old of
                                                          Nothing -> Just (e, e')
                                                          Just (o, o') -> Just (o, e')
                                                        in (new, ()))
           addListener er c w
           return tv

queryComponentListener :: ComponentListener -> IO (Maybe (EntityData, EntityData))
queryComponentListener tv = atomicModifyIORef tv (\val -> (Nothing, val))


{-
        A system reacts towards changes/updates in entities and their components. The system does it by creating for each component it handles an internal represenation, which it modifies upon changes to this component and to potentially additional changes in other components. For example an entity representing a light could have a Light component, which is created and modified upon change in the light component data. The entity may well have also a position component and the light entity is moved, in case the position component is modified.

        In general the system works by keeping a list of component listener on each added (each observes one component of an entity) and functions, which are called, in case the component listener exhibits a component change. 

-}

type OnUpdateFunction = EntityData -> EntityData -> IO ()
type OnDeleteFunction = IO ()
type SystemRecord = (Entity, Either (ComponentListener, OnUpdateFunction) OnDeleteFunction)
type SystemFunction = SystemData -> Entity -> IO [SystemRecord]

-- | this data specifies the type of system
data SystemData = SystemData {
     sdLock :: MVar (),
     sdNewEntities :: IORef [Entity], 
     sdDelEntities :: IORef [Entity],
     sdRecords :: [SystemRecord],
     sdComponents :: [Word64],
     sdProperties :: [Word64],
     sdEvents :: [Word64],

     sdCreateItem :: Word64 -> B.ByteString -> IO (
                            Int, 
                            Ptr () 
                            ),
     sdDestroyItem :: Word64 -> (Ptr ()) -> IO ((Int)),
     sdGetMessageSender :: Word64 -> Word64 -> IO ((Int), (FunPtr (Ptr () -> Ptr CChar -> CInt -> IO CInt))),
     sdRegisterMessageReceiver :: Word64 -> Word64 -> (Ptr ()) -> (FunPtr (Ptr () -> Ptr CChar -> CInt -> IO CInt)) -> IO Int,
     sdErrorMessage :: (Int) -> IO ((String))
}

addEntity :: SystemData -> Entity -> IO ()
addEntity sd entity = atomicModifyIORef (sdNewEntities sd) (\a -> (entity : a, ()))

removeEntity :: SystemData -> Entity -> IO ()
removeEntity sd entity = atomicModifyIORef (sdDelEntities sd) (\a -> (entity : a, ()))

stepRecord (er, Left (listener, updateF)) = do
  me <- queryComponentListener listener
  case me of
        Just (e, e') -> do
                          updateF e e'
                          return ()
--                          stepRecord (er, Left (listener, updateF))
        Nothing -> return ()
stepRecord (er, Right deleteF) = return ()

stepSystem :: SystemData -> IO Bool -> IO (SystemData, Bool)
stepSystem sd@(SystemData lock nrefs drefs records cs ps es ci di gm rm em) stepF = do

           -- add and delete Entitys
           adds <- atomicModifyIORef nrefs (\a -> ([],a))
           dels <- atomicModifyIORef drefs (\a -> ([],a))

           -- remove instances
           let delrecs = filter (\r -> (fst r) `elem` dels) records
           mapM (\r -> clearListeners (fst r)) delrecs
           mapM (\r -> case snd r of
                        Right delF ->  do
                                          delF  -- execute delete function
                                          return ()
                        _ -> return ()) delrecs
           let records' = filter (\r -> not ((fst r) `elem` dels)) records
           
           -- add new instances
           newRecords <- mapM (systemFunction sd) (reverse adds)
           let records'' = (concat newRecords) ++ records'

           -- run specific stepSystem
           qFlag <- stepF

           -- run stepfunction on tuples 
           mapM stepRecord records'' 

           -- return new values
           let newSD = (SystemData lock nrefs drefs records'' cs ps es ci di gm rm em)
           return (newSD, qFlag) -- need to add quit condition here
                 
runS s stepF stepT = do
    nowT <- getTime
    (s', qFlag) <- stepSystem s stepF
    if qFlag then do
      shutdownSystem s'
      return ()
      else do
        nowT' <- getTime
        let timeUsed = nowT' - nowT
        if timeUsed < stepT then do
          threadDelay ((fromIntegral . usec) (stepT - timeUsed) )
          else do
            return ()
        runS s' stepF stepT

-- | start a new thread, which runs the system
runSystem :: IO Bool -> GameTime -> SystemData -> IO ()
runSystem stepF stepT systemData = do
  forkOS  $ runS systemData  stepF stepT 
  return ()

-- called within the run loop

systemFunction sd entity = do

  let components = sdComponents sd
  let properties = sdProperties sd
  let events = sdEvents sd

  let createItem = sdCreateItem sd 
  let destroyItem = sdDestroyItem sd
  let getMessageSender = sdGetMessageSender sd
  let registerMessageReceiver = sdRegisterMessageReceiver sd
  let errorMessage = sdErrorMessage sd

  let r = []
  e <- readE entity

  -- create system records, (Entity, Either (Listener, UdateF) DeleteF)

  rs <- mapM (\c -> if c `elem` components
                        then do (res, rep) <- createItem c (e M.! c)   -- create the item
                                if res == 0 
                                    then do let ir = (entity, Right (destroyItem c rep >> return ()))

                                            -- add property records with listeners, also components can act as property
                                            prs <- mapM (\p -> if p `elem` (properties ++ components) 
                                                                    then do (res', propF) <- getMessageSender c p
                                                                            if res' == 0 
                                                                              then do lp <- componentListener entity (ComponentType p)
                                                                                      let uf = (\oe ne -> callMsgFunction propF rep (ne M.! p) >> return ())
                                                                                      uf e e   -- call message function, update component with this property right away
                                                                                      return [(entity, Left (lp, uf) )]
                                                                              else return []
                                                                    else return []
                                                          ) (M.keys e)

                                            let prs' = concat prs

                                            -- add event writer, in case applicable
                                            mapM (\p -> if p `elem` events 
                                                            then do let fwrite = (\_ msgData msgLen -> do   bData <- B.packCStringLen (msgData, fromIntegral msgLen)
                                                                                                            _setC' entity p bData
                                                                                                            return 0)
                                                                    fwritePtr <- mkMsgFunPtr fwrite
                                                                    registerMessageReceiver c p rep fwritePtr
                                                                    -- we would also need to delete the funptr later with "freeHaskellFunPtr" currently not implemented
                                                                    return ()
                                                            else return ()
                                                    ) (M.keys e)

                                            return $ (ir : prs')
                                    else return []

                        else return []
              ) (M.keys e) 

  return $ concat rs


shutdownSystem :: SystemData -> IO ()
shutdownSystem system = print "Shutdown System received" >> return ()


-- management of systems
--

-- | add an Entity to the world, thread safe        
addToWorld :: [SystemData] -> Entity -> IO ()
addToWorld systems e = mapM (\s -> addEntity s e) systems >> return () 

-- | remove an Entity from the world, thread safe        
removeFromWorld :: [SystemData] -> Entity -> IO ()
removeFromWorld systems e = mapM (\s -> removeEntity s e) systems >> return ()
    



                          
