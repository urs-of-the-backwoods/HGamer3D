{-
	Sound Source
	HGamer3D Library (A project to enable 3D game development in Haskell)
	Copyright 2015 Peter Althainz
	
	Distributed under the Apache License, Version 2.0
	(See attached file LICENSE or copy at 
	http://www.apache.org/licenses/LICENSE-2.0)
 
	file: HGamer3D/Audio/SoundSource.hs
-}

-- | Module providing a Sound Source
module HGamer3D.Audio.SoundSource
(
    SoundSource (..),
    ctSoundSource
)

where

import Fresco
import Data.MessagePack
import Debug.Trace
import Data.Text

import HGamer3D.Data

data SoundSource = Sound Text Float Bool Text -- resource, Volume, loop, volume group
                   | Sound3D Text Float Bool Text
                   | Music Text Float Bool Text
                   deriving (Eq, Show)
                   
instance ComponentClass SoundSource where
    toObj (Sound snd vol l g) = ObjectArray [ObjectInt 0, toObj snd, ObjectFloat vol, ObjectBool l, toObj g]
    toObj (Sound3D snd vol l g) = ObjectArray [ObjectInt 1, toObj snd, ObjectFloat vol, ObjectBool l, toObj g]
    toObj (Music snd vol l g) = ObjectArray [ObjectInt 2, toObj snd, ObjectFloat vol, ObjectBool l, toObj g]
    fromObj (ObjectArray [ObjectInt 0, snd_o, ObjectFloat vol, ObjectBool l, g_o]) = Sound (fromObj snd_o) vol l (fromObj g_o)
    fromObj (ObjectArray [ObjectInt 1, snd_o, ObjectFloat vol, ObjectBool l, g_o]) = Sound3D (fromObj snd_o) vol l (fromObj g_o)
    fromObj (ObjectArray [ObjectInt 2, snd_o, ObjectFloat vol, ObjectBool l, g_o]) = Music (fromObj snd_o) vol l (fromObj g_o)

ctSoundSource :: ComponentType SoundSource
ctSoundSource = ComponentType 0xafcef7aa41d88c0d
  

