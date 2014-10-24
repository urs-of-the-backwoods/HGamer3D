-- This source file is part of HGamer3D
-- (A project to enable 3D game development in Haskell)
-- For the latest info, see http://www.hgamer3d.org
--
-- (c) 2011-2013 Peter Althainz
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


-- Audio-BasicFunctions.hs


module Main where

import HGamer3D.Util
import HGamer3D.Audio.BaseAPI

import Control.Monad

playM = do
	mu <- musicAudioSource ("RMN-Music-Pack" ++ osSep ++ "OGG" ++ osSep ++ "CD 1 - Journey Begins" ++ osSep ++ "1-01 Town of Wishes.ogg")
	case mu of
		Just audio -> do
			playAudioSource audio
			return (Just audio)
		Nothing -> do
			return Nothing

playS = do
	mu <- soundAudioSource ("inventory_sound_effects" ++ osSep ++ "metal-clash.wav")
	case mu of
		Just audio -> do
			playAudioSource audio
			return (Just audio)
		Nothing -> do
			return Nothing

delAudio mu = do
	case mu of
		Nothing -> return ()
		Just audio -> do
			freeAudioSource audio
			return ()



main = do
	putStrLn "Choose sound (1) or music (2) test:"
	choice <- readLn
	if (toInteger choice) == 2 then do
		putStrLn "This test plays music, press a key"
		getChar
		
		ad <- playM
		
		putStrLn "press a key"
		getChar

		l <- delAudio ad
	
		return ()
		else
			if (toInteger choice) == 1 then do
				putStrLn "This test plays a sound, press a key"
				getChar
				
				ad <-  playS
				
				putStrLn "press a key"
				getChar
				
				l <- delAudio ad
				
				return ()
				else
					return ()
