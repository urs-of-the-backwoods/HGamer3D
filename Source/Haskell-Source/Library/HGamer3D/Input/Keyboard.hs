{-
	Keyboard functionality and settings
	HGamer3D Library (A project to enable 3D game development in Haskell)
	Copyright 2015 Peter Althainz
	
	Distributed under the Apache License, Version 2.0
	(See attached file LICENSE or copy at 
	http://www.apache.org/licenses/LICENSE-2.0)
 
	file: HGamer3D/Input/Keyboard.hs
-}

-- | Module providing the keyboard functionality and settings
module HGamer3D.Input.Keyboard

(
    KeyEvent (..),
    ctKeyEvent
)

where

import Data.MessagePack
import Debug.Trace
import Data.Text

import HGamer3D.Data

data KeyEvent =   KeyUp Int Int Text      -- Key, Scancode, Name
                | KeyDown Int Int Text    -- Key, Scancode, Name
                deriving (Eq, Show)
                
instance ComponentClass KeyEvent where
    toObj (KeyUp k s n) = ObjectArray [ObjectInt 1, ObjectInt (fromIntegral k), ObjectInt (fromIntegral s), toObj n]
    toObj (KeyDown k s n) = ObjectArray [ObjectInt 2, ObjectInt (fromIntegral k), ObjectInt (fromIntegral s), toObj n]
    fromObj (ObjectArray [ObjectInt 1, ObjectInt k, ObjectInt s, n_o]) = KeyUp (fromIntegral k) (fromIntegral s) (fromObj n_o)
    fromObj (ObjectArray [ObjectInt 2, ObjectInt k, ObjectInt s, n_o]) = KeyDown (fromIntegral k) (fromIntegral s) (fromObj n_o)
    
ctKeyEvent :: ComponentType KeyEvent
ctKeyEvent = ComponentType 0x5ba1617fb50e97e5


    