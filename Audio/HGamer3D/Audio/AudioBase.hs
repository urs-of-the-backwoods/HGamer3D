{-# OPTIONS_HADDOCK hide #-}

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

-- | Audio functionality for HGamer3D, internal implementation. This module also exports internal data declarations, in case they are needed. Normally you should only use HGamer3D.Audio and import that, which exhibits the public API.
module HGamer3D.Audio.AudioBase
where

import GHC.Ptr

import HGamer3D.Data
import HGamer3D.Data.HG3DClass

import HGamer3D.Common

import HGamer3D.Bindings.SFML.ClassPtr
import HGamer3D.Bindings.SFML.Utils

import qualified HGamer3D.Bindings.SFML.ClassListener as Listener
import qualified HGamer3D.Bindings.SFML.ClassMusic as Music 
import qualified HGamer3D.Bindings.SFML.ClassSound as Sound
import qualified HGamer3D.Bindings.SFML.ClassSoundBuffer as SoundBuffer
import qualified HGamer3D.Bindings.SFML.ClassSoundSource as SoundSource
import qualified HGamer3D.Bindings.SFML.ClassSoundStream as SoundStream


import HGamer3D.Bindings.SFML.EnumSoundSourceStatus
import HGamer3D.Bindings.SFML.EnumSoundSourceStatus

import Control.Monad
import qualified Data.Map as M
import Data.Maybe


-- Source types

data AudioSource = Music HG3DClass | Sound HG3DClass HG3DClass

-- Listener Functions

-- | set overall main audio volume
setAudioMainVolume :: Float -- ^ volume in a scale between 0 and 100.0
					-> IO ()
setAudioMainVolume vol = Listener.setGlobalVolume vol

-- | get overall main audio volume
getAudioMainVolume :: IO Float -- ^ volume in a scale between 0 and 100.0
getAudioMainVolume = Listener.getGlobalVolume

-- | set position of the listener, this is where the \"micro\" is located
setAudioListenerPosition :: Vec3 -- ^ position of listener
							-> IO ()
setAudioListenerPosition (Vec3 x y z) = Listener.setPosition x y z

-- | set direction of the listener
setAudioListenerDirection :: Vec3 -- ^ direction, in which listener hears
                             -> IO ()
setAudioListenerDirection (Vec3 x y z) = Listener.setDirection x y z


-- Music Functions, deletion and cretion of Audio types
--

_findMusicFile filename = do
        dirs <- liftM (fmap (++ osSep ++ "sound")) (sequence [getAppMediaDirectory])
        filepath <- findFileInDirs filename dirs
        return filepath
  
  

-- | create music (stream source, reading from file)
musicAudioSource :: String -- ^ filename of music 
			-> IO (Maybe AudioSource) -- ^ audio source, which can be played
musicAudioSource filename = do
	music <- Music.new
        -- try to find file in hg3d directories
        filepath <-_findMusicFile filename
	-- try to open
        case filepath of
          Just f -> do
            fOk <- Music.openFromFile music (f)
            if fOk then
		return (Just (Music music))
		else do
                      Music.delete music
                      return Nothing
          Nothing -> return Nothing

-- | create sound (stream source, loaded into memory)
soundAudioSource :: String -- ^ filename of sound without path
					-> IO (Maybe AudioSource) -- ^ audio source, which can be played
soundAudioSource filename = do
	sound <- Sound.new
	buffer <- SoundBuffer.new
        filepath <-_findMusicFile filename
	-- try to open
        case filepath of
          Just f -> do
            fOk <- SoundBuffer.loadFromFile buffer (f)
            if fOk then do
		Sound.setBuffer sound buffer		
		return (Just (Sound sound buffer))
		else do
                    Sound.delete sound
                    SoundBuffer.delete buffer
                    return Nothing
          Nothing -> return Nothing

-- | delete audio source
freeAudioSource :: AudioSource -> IO ()
freeAudioSource (Music music) = do
	Music.delete music
deleteAudioSource (Sound sound buffer) = do
	Sound.delete sound
	SoundBuffer.delete buffer


-- Source play, pause, stop
--

-- | starts playing audio source
playAudioSource :: AudioSource -- ^the audio source
					-> IO ()
playAudioSource (Music music) = do
	SoundStream.play music
playAudioSource (Sound sound buffer) = do
	Sound.play sound

-- | pause playing audio source, remembers location
pauseAudioSource :: AudioSource -> IO ()
pauseAudioSource (Music music) = do
	SoundStream.pause music
pauseAudioSource (Sound sound buffer) = do
	Sound.pause sound

-- | completely stops audio playing
stopAudioSource :: AudioSource -> IO ()
stopAudioSource (Music music) = do
	SoundStream.stop music
stopAudioSource (Sound sound buffer) = do
	Sound.stop sound

-- | is audio play looping
getAudioSourceLoop :: AudioSource -- ^audio source
				-> IO Bool -- ^true, loop is enabled, false loop is disabled
getAudioSourceLoop (Music music) = do
	fLoop <- SoundStream.getLoop music
	return fLoop
getAudioSourceLoop (Sound sound buffer) = do
	fLoop <- Sound.getLoop sound
	return fLoop

-- | set audio play is looping
setAudioSourceLoop :: AudioSource -- ^audio source
				-> Bool -- ^loop enabled
				-> IO ()
setAudioSourceLoop (Music music) fLoop = do
	SoundStream.setLoop music fLoop
setAudioSourceLoop (Sound sound buffer) fLoop = do
	Sound.setLoop sound fLoop


-- Sound Source Functions
--

-- | get volume of a single audio source
getAudioSourceVolume :: AudioSource -- ^audio source
					-> IO Float -- ^volume of audio source in scale of 0 to 100.0
					
getAudioSourceVolume (Music s) = SoundSource.getVolume s
getAudioSourceVolume (Sound s b) = SoundSource.getVolume s

-- | set volume of a single audio source
setAudioSourceVolume :: AudioSource -- ^audio source
				-> Float -> IO () -- ^volume of this audio source in scale of 0 to 100.0
				
setAudioSourceVolume (Music s) v = SoundSource.setVolume s v
setAudioSourceVolume (Sound s b) v = SoundSource.setVolume s v

-- | get audio source pitch
-- the pitch is the frequency of the sound which also influences playing speed. The default value is 1.0.
getAudioSourcePitch :: AudioSource -- ^audio source
				-> IO Float -- ^pitch, a 1.0 is no change to original sound
				
getAudioSourcePitch (Music s) = SoundSource.getPitch s
getAudioSourcePitch (Sound s b) = SoundSource.getPitch s

-- | sets the pitch of an audio source
setAudioSourcePitch :: AudioSource -> Float -> IO ()
setAudioSourcePitch (Music s) p = SoundSource.setPitch s p
setAudioSourcePitch (Sound s b) p = SoundSource.setPitch s p

-- | sets the position of an audio source
setAudioSourcePosition :: AudioSource -- ^audio source
					-> Vec3 -> IO () -- ^new position of this source
					
setAudioSourcePosition (Music s) (Vec3 x y z) = SoundSource.setPosition s x y z
setAudioSourcePosition (Sound s b) (Vec3 x y z) = SoundSource.setPosition s x y z

-- | gets the position of an audio source
getAudioSourcePosition :: AudioSource -- ^audio source
					-> IO Vec3 -- ^position of the audio source
					
getAudioSourcePosition (Music s) = SoundSource.getPosition s
getAudioSourcePosition (Sound s b) = SoundSource.getPosition s

-- | gets the attenuation factor of an audio source
getAudioSourceAttenuation :: AudioSource -- ^audio source
			-> IO Float -- ^attenuation is the damping factor by distance, default value is 1.0, 0.0 means no damping
getAudioSourceAttenuation (Music s) = SoundSource.getAttenuation s
getAudioSourceAttenuation (Sound s b) = SoundSource.getAttenuation s

-- | sets the attenuation factor of an audio source
setAudioSourceAttenuation :: AudioSource -- ^audio source
				-> Float -> IO () -- ^attenuation factor
setAudioSourceAttenuation (Music s) a = SoundSource.setAttenuation s a
setAudioSourceAttenuation (Sound s b) a = SoundSource.setAttenuation s a

-- | gets audio source min distance, distance at which sound is played a volume 100.0
getAudioSourceMinDistance :: AudioSource -> IO Float
getAudioSourceMinDistance (Music s) = SoundSource.getMinDistance s
getAudioSourceMinDistance (Sound s b) = SoundSource.getMinDistance s

-- | sets audio source min distance, distance at which sound is played a volume 100.0
setAudioSourceMinDistance :: AudioSource -> Float -> IO ()
setAudioSourceMinDistance (Music s) d = SoundSource.setMinDistance s d
setAudioSourceMinDistance (Sound s b) d = SoundSource.setMinDistance s d

-- | set flag if audio source depends on position, default is FALSE (not dependent on position)
setAudioSourcePositionDependent :: AudioSource -> Bool -> IO ()
setAudioSourcePositionDependent (Music s) isR = SoundSource.setRelativeToListener s isR
setAudioSourcePositionDependent (Sound s b) isR = SoundSource.setRelativeToListener s isR

-- | get flag if audio source depends on position, default is FALSE (not dependent on position)
isAudioSourcePositionDependent :: AudioSource -> IO Bool
isAudioSourcePositionDependent (Music s) = SoundSource.isRelativeToListener s
isAudioSourcePositionDependent (Sound s b) = SoundSource.isRelativeToListener s

