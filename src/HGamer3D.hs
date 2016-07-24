-- This source file is part of HGamer3D
-- (A project to enable 3D game development in Haskell)
-- For the latest info, see http://www.hgamer3d.org
-- 
-- (c) 2011 - 2015 Peter Althainz
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
-- 
-- HGamer3D
-- 

-- | Main module, to include API
module HGamer3D

(
    module Fresco,
    module HGamer3D.Data,
    module HGamer3D.Util,
    module HGamer3D.Graphics3D,
    module HGamer3D.Input,
    module HGamer3D.GUI,
    module HGamer3D.Audio,

    HG3D,
    GameLogicFunction,
    runGame,

    registerCallback,
    isExitHG3D,
    resetExitHG3D,
    exitHG3D,
    createE,
)

where

import Fresco
import HGamer3D.Data
import HGamer3D.Util
import HGamer3D.Graphics3D
import HGamer3D.Input
import HGamer3D.GUI
import HGamer3D.Audio

import Control.Concurrent
import Control.Monad
import Control.Concurrent.MVar
import Data.IORef

-- Opaque Value to denote some game-loop data
data HG3D = HG3D ObjectLibSystem CallbackSystem (Var Bool)

-- runHG3D runs the engine in the main loop (for Mac) and executes game logic
type GameLogicFunction = HG3D -> IO ()

-- runGame, runs the game in the main loop, creates threads for GameLogicFunctions
runGame :: Graphics3DConfig -> GameLogicFunction -> GameTime -> IO ()
runGame conf glf loopSleepTime = do

    ols <- createOLS
    cbs <- createCBS
    varExit <- makeVar False

    let hg3d = HG3D ols cbs varExit

    forkIO $ do

        -- create graphics system
        eG3D <- createE hg3d [
            ctGraphics3DConfig #: conf,
            ctGraphics3DCommand #: NoCmd
            ]

        eih <- createE hg3d [
            ctInputEventHandler #: DefaultEventHandler,
            ctExitRequestedEvent #: ExitRequestedEvent
            ]

        -- create callback loop, handle windows exit command
        forkIO $ do
            registerReceiverCBS cbs eih ctExitRequestedEvent (\_ -> writeVar varExit True >> return ())
            forever $ (stepCBS cbs)

        -- create game logic loop
        forkIO $ glf hg3d

        -- create game step loop
        let gameStep = do
            setC eG3D ctGraphics3DCommand Step
            sleepFor loopSleepTime
            gameStep

        forkIO $ gameStep

        return ()

    -- enter into endless game loop
    let loopGame = do
        stepOLS ols
        ex <- readVar varExit
        if ex
            then return ()
            else loopGame

    loopGame

isExitHG3D (HG3D ols cbs varExit) = do
    ise <- readVar varExit
    return ise

resetExitHG3D (HG3D ols cbs varExit) = writeVar varExit False

exitHG3D (HG3D ols cbs varExit) = do
    writeVar varExit True >> return ()

registerCallback (HG3D ols cbs varExit) e ct f = do
    registerReceiverCBS cbs e ct f

createE (HG3D ols cbs varExit) creationList = do
    e <- newE creationList
    addEntityOLS ols e
    return e


