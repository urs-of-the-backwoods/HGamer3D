{-# Language StandaloneDeriving, DeriveDataTypeable #-}
{-# OPTIONS_HADDOCK hide #-}

-- This source file is part of HGamer3D
-- (A project to enable 3D game development in Haskell)
-- For the latest info, see http://www.hgamer3d.org
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

module HGamer3D.InputSystem.SystemInputSystem

where

import Data.Maybe
import Data.Typeable
import Data.Dynamic
--import Data.Traversable as T
import Data.IORef
import Control.Concurrent
import Control.Monad

import HGamer3D.Data
import HGamer3D.Common
import qualified HGamer3D.BaseAPI.InputSystem as IS
import HGamer3D.InputSystem.Event
import HGamer3D.InputSystem.Joystick
import HGamer3D.InputSystem.Mouse

updateJoystickStatus :: ECSInputSystem -> IO (Bool, [IS.Joystick])
updateJoystickStatus system = do
    let rJS = connectedJoysticks system
    oldJs <- readIORef rJS
    IS.updateJoystickStatus
    newJs <- IS.getConnectedJoysticks
    if oldJs /= newJs 
       then do
            writeIORef rJS newJs
            return (True, newJs)
       else return (False, newJs)
    
-- doCmd, handle commands, available for network nodes

doCmd :: ECSInputSystem -> ERef -> Maybe InputSystemCommand -> IO ()
doCmd system eref cmd = do
      case cmd of
           Just GetConnectedJoysticks -> do
                                 (newFlag, js) <- updateJoystickStatus system
                                 sendEvt eref (JoysticksConnected js)
           _ -> return ()


-- the input-system system data
                                 
data ECSInputSystem = ECSInputSystem {
     connectedJoysticks :: IORef [IS.Joystick],
     joystickInfos :: IORef [ERef],
     mouseValues :: IORef [ERef]
     }
     
instance System ECSInputSystem where

    initializeSystem = do
         
      lock <- newMVar ()
      newERefs <- newIORef []
      delERefs <- newIORef []
      let records = []
      cjs <- newIORef []
      jinfos <- newIORef []
      mvals <- newIORef []
      let system = ECSInputSystem cjs jinfos mvals

      let systemFunction system eref = do

          let ECSInputSystem cjs jref mref = system
          e <- readE eref -- this e is used to create the representation

          if e #? CTJoI then do
             jis <- readIORef jref
             writeIORef jref (eref : jis)

             let createRecord ct = do
                 if e #? ct then do
                    l <- componentListener eref ct
                    let uf = case ct of
                                  CTCmd -> \ _ e' -> do
                                                              doCmd system eref (e' ?# CTCmd)
                                                              return ()
                                  _ -> \ _ _ -> return ()

                    let df = return ()
                    return $ Just (l, uf, df)
                    else return Nothing

             newRecords <- Prelude.mapM createRecord [CTCmd]
             return (map fromJust (filter isJust newRecords))

             else if e #? CTMou then do
                  mis <- readIORef mref
                  writeIORef mref (eref : mis)
                  return []
                  else return []

      return (SystemData lock newERefs delERefs records system systemFunction)


    stepSystem (SystemData lock newERefs delERefs records system systemFunction) = do

      -- update joystick status
      (newFlag, js) <- updateJoystickStatus system
      if newFlag then do
         jrefs <- readIORef (joystickInfos system)
         mapM (\eref -> sendEvt eref (JoysticksConnected js)) jrefs >> return ()
         else return ()

      -- update joystick values
      jinfos <- readIORef (joystickInfos system)
      mapM (\eref -> do
                  e <- readE eref
                  if e #? CTJoV then do
                     let (JoystickInfo joystick axes buttons) = (e # CTJoI)
                     if elem joystick js then do
                        aVals <- mapM (\a -> IS.getJoystickAxisPosition joystick a) axes
                        bVals <- mapM (\b -> IS.isJoystickButtonPressed joystick b) buttons
                        updateE eref CTJoV (const (JoystickValue True aVals bVals))
                        else
                                updateE eref CTJoV (const (JoystickValue False [] []))
                      else return ()
           ) jinfos      

      -- update mouse values
      mvals <- readIORef (mouseValues system)
      mapM (\mVal -> do
                  (x, y) <- IS.getMousePosition
                  bs <- filterM (\b -> IS.isMouseButtonPressed b) [IS.MouseButtonLeft, IS.MouseButtonRight, IS.MouseButtonMiddle, IS.MouseButtonXButton1, IS.MouseButtonXButton2]
                  updateE mVal CTMou (const (MouseValue x y bs))
           ) mvals

      return False

forkInputSystemWorld :: GameTime -> IO [SomeSystem]
forkInputSystemWorld sleepT = do
                       system <- (runSystem sleepT) :: IO (SystemData ECSInputSystem)
                       return $ [SomeSystem system]


{-

world <- forkInputSystemWorld (msecT 100)
mE <- newE [ CTMou #: () ]
addToWorld world mE
let loop = readE mE >>= \e' -> return (e' # CTMou :: MouseValue) >>= print . show >> loop
t <- forkIO loop
killThread t

j <- newE [ CTJoI #: (JoystickInfo (IS.Joystick 1) [IS.JoystickAxisX] [IS.JoystickButton 0]), CTJoV #: ()]
addToWorld world j
eH = \me -> case me of 
                    (JoysticksConnected js) -> print js
                    _ -> return ()
regEvtH j eH 

readE j >>= \e -> return (e # CTJoV :: JoystickValue)

-}
