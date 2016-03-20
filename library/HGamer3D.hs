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

    configureHG3D,
    stepHG3D,
    loopHG3D,
    registerCallback,
    isExitHG3D,
    exitHG3D,

    HG3D,
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

-- run loop

type HG3D = (Entity, CallbackSystem, Var Bool)

configureHG3D = do

	cbsRef <- newEmptyMVar

	-- create graphics system
	eG3D <- newE [
	    ctGraphics3DConfig #: standardGraphics3DConfig,
	    ctGraphics3DCommand #: NoCmd
	    ]

	eih <- newE [
	    ctInputEventHandler #: DefaultEventHandler,
	    ctExitRequestedEvent #: ExitRequestedEvent
	    ]

	varExit <- makeVar False

	-- create callback loop
	forkIO $ do
	    cbs <- createCBS
	    registerCallback (eG3D, cbs, varExit) eih ctExitRequestedEvent (\_ -> writeVar varExit True >> return ())
	    putMVar cbsRef cbs
	    forever (stepCBS cbs)

	cbs <- takeMVar cbsRef

	return (eG3D, cbs, varExit)

stepHG3D (eG3D, cbs, varExit) = do
    setC eG3D ctGraphics3DCommand Step

isExitHG3D (eG3D, cbs, varExit) = do
	ise <- readVar varExit
	return ise

resetExitHG3D (eG3D, cbs, varExit) = writeVar varExit False

loopHG3D hg3d loopSleepTime checkExit = do
	stepHG3D hg3d
	sleepFor loopSleepTime
	ise <- do
		ise' <- isExitHG3D hg3d
		if ise' then do
			resetExitHG3D hg3d
			checkExit
			else
				return False
	if not ise then do
		loopHG3D hg3d loopSleepTime checkExit
		return ()
		else
			return ()

exitHG3D (eG3D, cvs, varExit) = do
	writeVar varExit True >> return ()

registerCallback (eG3D, cbs, varExit) e ct f = do
	registerReceiverCBS cbs e ct f