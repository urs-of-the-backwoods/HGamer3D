-- This source file is part of HGamer3D
-- (A project to enable 3D game development in Haskell)
-- For the latest info, see http://www.althainz.de/HGamer3D.html
--
-- (c) 2011 Peter Althainz
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

import HGamer3D.BaseAPI

import Control.Monad

playM = do
	mu <- createMusic "orchestral.ogg"
	case mu of
		Just audio -> do
			playAudioSource audio
			return (Just audio)
		Nothing -> do
			return Nothing

playS = do
	mu <- createSound "canary.wav"
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
			deleteAudioSource audio
			return ()



main = do
	-- init hgamer3d
	hg3d <- initHGamer3D "HGamer3D - Audio Example" 
	
	putStrLn "Choose sound (1) or music (2) test:"
	choice <- readLn
	if (toInteger choice) == 2 then do
		putStrLn "This test plays music, press a key"
		getChar
		
		(ad, hg3d) <- runMHGamer3D hg3d playM
		
		putStrLn "press a key"
		getChar

		(l, hg3d) <- runMHGamer3D hg3d (delAudio ad)
	
		return ()
		else
			if (toInteger choice) == 1 then do
				putStrLn "This test plays a sound, press a key"
				getChar
				
				( ad, hg3d) <- runMHGamer3D hg3d playS
				
				putStrLn "press a key"
				getChar
				
				( l, hg3d) <- runMHGamer3D hg3d (delAudio ad)
				
				return ()
				else
					return ()
