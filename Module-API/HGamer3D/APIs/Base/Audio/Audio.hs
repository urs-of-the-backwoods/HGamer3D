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

-- Audio.hs

-- |The Audio functionality of the Base API.


module HGamer3D.APIs.Base.Audio.Audio

(
	-- * Types
	module HGamer3D.Bindings.SFML.EnumSoundSourceStatus,
	AudioSource (..), 
	
	-- * Listener Functions
	setAudioMainVolume,
	getAudioMainVolume,
	setAudioListenerPosition,
	setAudioListenerDirection,
	
	-- * Source creation and delete Functions
	createMusic,
	createSound,
	deleteAudioSource,
	
	-- * Source play function
	playAudioSource,
	pauseAudioSource,
	stopAudioSource,

	-- * Source property functions
	setAudioSourceLoop,
	getAudioSourceLoop,

	getAudioSourceVolume,
	setAudioSourceVolume,

	getAudioSourcePitch,
	setAudioSourcePitch,

	getAudioSourcePosition,
	setAudioSourcePosition,

	getAudioSourceAttenuation,
	setAudioSourceAttenuation,

	getAudioSourceMinDistance,
	setAudioSourceMinDistance,

	setAudioSourcePositionDependent,
	isAudioSourcePositionDependent,
	
)

where

import GHC.Ptr

import HGamer3D.APIs.Base.Engine.Types

import HGamer3D.Bindings.SFML.ClassPtr
import HGamer3D.Bindings.SFML.Utils

import qualified HGamer3D.Bindings.SFML.ClassListener as Listener
import qualified HGamer3D.Bindings.SFML.ClassMusic as Music 
import qualified HGamer3D.Bindings.SFML.ClassSound as Sound
import qualified HGamer3D.Bindings.SFML.ClassSoundBuffer as SoundBuffer
import qualified HGamer3D.Bindings.SFML.ClassSoundSource as SoundSource
import qualified HGamer3D.Bindings.SFML.ClassSoundStream as SoundStream
import HGamer3D.Bindings.SFML.EnumSoundSourceStatus

import HGamer3D.Data.HG3DClass
import Control.Monad.Trans
import Control.Monad.Reader
import HGamer3D.Data.Vector

-- Source types

data AudioSource = Music HG3DClass | Sound HG3DClass HG3DClass


-- Listener Functions

-- | set overall main audio volume
setAudioMainVolume :: Float -- ^ volume in a scale between 0 and 100.0
					-> MHGamer3D ()
setAudioMainVolume vol = liftIO $ Listener.setGlobalVolume vol

-- | get overall main audio volume
getAudioMainVolume :: MHGamer3D Float -- ^ volume in a scale between 0 and 100.0
getAudioMainVolume = liftIO $ Listener.getGlobalVolume

-- | set position of the listener, this is where the \"micro\" is located
setAudioListenerPosition :: Vec3 -- ^ position of listener
							-> MHGamer3D ()
setAudioListenerPosition (Vec3 x y z) = liftIO $ Listener.setPosition x y z

-- | set direction of the listener
setAudioListenerDirection :: Vec3 -- ^ direction, in which listener hears
                             -> MHGamer3D ()
setAudioListenerDirection (Vec3 x y z) = liftIO $ Listener.setDirection x y z


-- Music Functions, deletion and cretion of Audio types
--

-- | create music (stream source, reading from file)
createMusic :: String -- ^ filename of music without path
			-> MHGamer3D (Maybe AudioSource) -- ^ audio source, which can be played
createMusic filename = do
	music <- liftIO $ Music.new
	rs <- ask
	let cs = commonSystem rs
	let dir = (csHG3DPath cs)
	let progdir = (csProgPath cs)
	-- first try in runtime folder
	fOk <- liftIO $ Music.openFromFile music (dir ++ "\\media\\sound\\" ++ filename)
	if fOk then
		return (Just (Music music))
		else do
			fOk <- liftIO $ Music.openFromFile music (progdir ++ "\\media\\sound\\" ++ filename)
			if fOk then
				return (Just (Music music))
				else do
					liftIO $ Music.delete music
					return Nothing

-- | create sound (stream source, loaded into memory)
createSound :: String -- ^ filename of sound without path
					-> MHGamer3D (Maybe AudioSource) -- ^ audio source, which can be played
createSound filename = do
	sound <- liftIO $ Sound.new
	buffer <- liftIO $ SoundBuffer.new
	rs <- ask
	let cs = commonSystem rs
	let dir = (csHG3DPath cs)
	let progdir = (csProgPath cs)
	fOk <- liftIO $ SoundBuffer.loadFromFile buffer (dir ++ "\\media\\sound\\" ++ filename)
	if fOk then do
		liftIO $ Sound.setBuffer sound buffer		
		return (Just (Sound sound buffer))
		else do
		fOk <- liftIO $ SoundBuffer.loadFromFile buffer (progdir ++ "\\media\\sound\\" ++ filename)
		if fOk then do
			liftIO $ Sound.setBuffer sound buffer		
			return (Just (Sound sound buffer))
			else do
				liftIO $ Sound.delete sound
				liftIO $ SoundBuffer.delete buffer
				return Nothing

-- | delete audio source
deleteAudioSource :: AudioSource -> MHGamer3D ()
deleteAudioSource (Music music) = do
	liftIO $ Music.delete music
deleteAudioSource (Sound sound buffer) = do
	liftIO $ Sound.delete sound
	liftIO $ SoundBuffer.delete buffer


-- Source play, pause, stop
--

-- | starts playing audio source
playAudioSource :: AudioSource -- ^the audio source
					-> MHGamer3D ()
playAudioSource (Music music) = do
	liftIO $ SoundStream.play music
playAudioSource (Sound sound buffer) = do
	liftIO $ Sound.play sound

-- | pause playing audio source, remembers location
pauseAudioSource :: AudioSource -> MHGamer3D ()
pauseAudioSource (Music music) = do
	liftIO $ SoundStream.pause music
pauseAudioSource (Sound sound buffer) = do
	liftIO $ Sound.pause sound

-- | completely stops audio playing
stopAudioSource :: AudioSource -> MHGamer3D ()
stopAudioSource (Music music) = do
	liftIO $ SoundStream.stop music
stopAudioSource (Sound sound buffer) = do
	liftIO $ Sound.stop sound

-- | is audio play looping
getAudioSourceLoop :: AudioSource -- ^audio source
				-> MHGamer3D Bool -- ^true, loop is enabled, false loop is disabled
getAudioSourceLoop (Music music) = do
	fLoop <- liftIO $ SoundStream.getLoop music
	return fLoop
getAudioSourceLoop (Sound sound buffer) = do
	fLoop <- liftIO $ Sound.getLoop sound
	return fLoop

-- | set audio play is looping
setAudioSourceLoop :: AudioSource -- ^audio source
				-> Bool -- ^loop enabled
				-> MHGamer3D ()
setAudioSourceLoop (Music music) fLoop = do
	liftIO $ SoundStream.setLoop music fLoop
setAudioSourceLoop (Sound sound buffer) fLoop = do
	liftIO $ Sound.setLoop sound fLoop


-- Sound Source Functions
--

-- | get volume of a single audio source
getAudioSourceVolume :: AudioSource -- ^audio source
					-> MHGamer3D Float -- ^volume of audio source in scale of 0 to 100.0
					
getAudioSourceVolume (Music s) = liftIO $ SoundSource.getVolume s
getAudioSourceVolume (Sound s b) = liftIO $ SoundSource.getVolume s

-- | set volume of a single audio source
setAudioSourceVolume :: AudioSource -- ^audio source
				-> Float -> MHGamer3D () -- ^volume of this audio source in scale of 0 to 100.0
				
setAudioSourceVolume (Music s) v = liftIO $ SoundSource.setVolume s v
setAudioSourceVolume (Sound s b) v = liftIO $ SoundSource.setVolume s v

-- | get audio source pitch
-- the pitch is the frequency of the sound which also influences playing speed. The default value is 1.0.
getAudioSourcePitch :: AudioSource -- ^audio source
				-> MHGamer3D Float -- ^pitch, a 1.0 is no change to original sound
				
getAudioSourcePitch (Music s) = liftIO $ SoundSource.getPitch s
getAudioSourcePitch (Sound s b) = liftIO $ SoundSource.getPitch s

-- | sets the pitch of an audio source
setAudioSourcePitch :: AudioSource -> Float -> MHGamer3D ()
setAudioSourcePitch (Music s) p = liftIO $ SoundSource.setPitch s p
setAudioSourcePitch (Sound s b) p = liftIO $ SoundSource.setPitch s p

-- | sets the position of an audio source
setAudioSourcePosition :: AudioSource -- ^audio source
					-> Vec3 -> MHGamer3D () -- ^new position of this source
					
setAudioSourcePosition (Music s) (Vec3 x y z) = liftIO $ SoundSource.setPosition s x y z
setAudioSourcePosition (Sound s b) (Vec3 x y z) = liftIO $ SoundSource.setPosition s x y z

-- | gets the position of an audio source
getAudioSourcePosition :: AudioSource -- ^audio source
					-> MHGamer3D Vec3 -- ^position of the audio source
					
getAudioSourcePosition (Music s) = liftIO $ SoundSource.getPosition s
getAudioSourcePosition (Sound s b) = liftIO $ SoundSource.getPosition s

-- | gets the attenuation factor of an audio source
getAudioSourceAttenuation :: AudioSource -- ^audio source
			-> MHGamer3D Float -- ^attenuation is the damping factor by distance, default value is 1.0, 0.0 means no damping
getAudioSourceAttenuation (Music s) = liftIO $ SoundSource.getAttenuation s
getAudioSourceAttenuation (Sound s b) = liftIO $ SoundSource.getAttenuation s

-- | sets the attenuation factor of an audio source
setAudioSourceAttenuation :: AudioSource -- ^audio source
				-> Float -> MHGamer3D () -- ^attenuation factor
setAudioSourceAttenuation (Music s) a = liftIO $ SoundSource.setAttenuation s a
setAudioSourceAttenuation (Sound s b) a = liftIO $ SoundSource.setAttenuation s a

-- | gets audio source min distance, distance at which sound is played a volume 100.0
getAudioSourceMinDistance :: AudioSource -> MHGamer3D Float
getAudioSourceMinDistance (Music s) = liftIO $ SoundSource.getMinDistance s
getAudioSourceMinDistance (Sound s b) = liftIO $ SoundSource.getMinDistance s

-- | sets audio source min distance, distance at which sound is played a volume 100.0
setAudioSourceMinDistance :: AudioSource -> Float -> MHGamer3D ()
setAudioSourceMinDistance (Music s) d = liftIO $ SoundSource.setMinDistance s d
setAudioSourceMinDistance (Sound s b) d = liftIO $ SoundSource.setMinDistance s d

-- | set flag if audio source depends on position, default is FALSE (not dependent on position)
setAudioSourcePositionDependent :: AudioSource -> Bool -> MHGamer3D ()
setAudioSourcePositionDependent (Music s) isR = liftIO $ SoundSource.setRelativeToListener s isR
setAudioSourcePositionDependent (Sound s b) isR = liftIO $ SoundSource.setRelativeToListener s isR

-- | get flag if audio source depends on position, default is FALSE (not dependent on position)
isAudioSourcePositionDependent :: AudioSource -> MHGamer3D Bool
isAudioSourcePositionDependent (Music s) = liftIO $ SoundSource.isRelativeToListener s
isAudioSourcePositionDependent (Sound s b) = liftIO $ SoundSource.isRelativeToListener s


