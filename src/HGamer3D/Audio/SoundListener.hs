{-
	Audio: Listener for sound, specifies mainyl the position for 3D sound
	HGamer3D Library (A project to enable 3D game development in Haskell)
	Copyright 2015 Peter Althainz
	
	Distributed under the Apache License, Version 2.0
	(See attached file LICENSE or copy at 
	http://www.apache.org/licenses/LICENSE-2.0)
 
	file: HGamer3D/Audio/SoundListener.hs
-}

-- | Sound Listener
module HGamer3D.Audio.SoundListener
where

import Fresco

type SoundListener = ()

ctSoundListener :: ComponentType SoundListener
ctSoundListener = ComponentType 0x7aacf4ee5bd2f958
