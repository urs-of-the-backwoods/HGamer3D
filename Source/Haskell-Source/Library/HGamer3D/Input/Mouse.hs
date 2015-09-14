{-
	Mouse functionality and settings
	HGamer3D Library (A project to enable 3D game development in Haskell)
	Copyright 2015 Peter Althainz
	
	Distributed under the Apache License, Version 2.0
	(See attached file LICENSE or copy at 
	http://www.apache.org/licenses/LICENSE-2.0)
 
	file: HGamer3D/Input/Mouse.hs
-}

-- | Module providing the Mouse functionality and settings
module HGamer3D.Input.Mouse
(
    MouseMode (..),
    Mouse (..),
    MouseEvent (..),
    ctMouse,
    ctMouseEvent
)

where

import Data.MessagePack
import Debug.Trace
import Data.Text

import HGamer3D.Data


-- Mouse mode and Mouse datatype

data MouseMode = MMAbsolute
                | MMRelative
                | MMWrap
                deriving (Eq, Show)
                
instance ComponentClass MouseMode where
    toObj MMAbsolute = ObjectInt 1
    toObj MMRelative = ObjectInt 2
    toObj MMWrap = ObjectInt 3
    fromObj (ObjectInt 1) = MMAbsolute
    fromObj (ObjectInt 2) = MMRelative
    fromObj (ObjectInt 3) = MMWrap
    
data Mouse = Mouse MouseMode deriving (Eq, Show) 

instance ComponentClass Mouse where
    toObj (Mouse mm) = ObjectArray [ObjectInt 1, toObj mm]
    fromObj (ObjectArray [ObjectInt 1, mm_o]) = Mouse (fromObj mm_o)

ctMouse :: ComponentType Mouse
ctMouse = ComponentType 0xa532f43b1c1c6bc7

-- Mouse Events

data MouseEvent =     MouseButtonUp Int Int Int         -- Button, Buttons, Qualifiers
                    | MouseButtonDown Int Int Int       -- Button, Buttons, Qualifiers
                    | MouseMove Int Int Int Int Int Int -- X, Y, DX, DY, Buttons, Qualifiers
                    | MouseWheel Int Int Int            -- Wheel, Buttons, Qualifiers
                    | MouseVisibleChanged Bool          -- Visible
                      deriving (Eq, Show)

instance ComponentClass MouseEvent where
    toObj (MouseButtonUp b bs qs) = ObjectArray [ObjectInt 1, ObjectInt (fromIntegral b), ObjectInt (fromIntegral bs), ObjectInt (fromIntegral qs)]
    toObj (MouseButtonDown b bs qs) = ObjectArray [ObjectInt 2, ObjectInt (fromIntegral b), ObjectInt (fromIntegral bs), ObjectInt (fromIntegral qs)]
    toObj (MouseMove x y dx dy bs qs) = ObjectArray [ObjectInt 3, ObjectInt (fromIntegral x), ObjectInt (fromIntegral y), ObjectInt (fromIntegral dx), ObjectInt (fromIntegral dy), ObjectInt (fromIntegral bs), ObjectInt (fromIntegral qs)]
    toObj (MouseWheel w bs qs) = ObjectArray [ObjectInt 4, ObjectInt (fromIntegral w), ObjectInt (fromIntegral bs), ObjectInt (fromIntegral qs)]
    toObj (MouseVisibleChanged v) = ObjectArray [ObjectInt 5, ObjectBool v]
    
    fromObj (ObjectArray [ObjectInt 1, ObjectInt b, ObjectInt bs, ObjectInt qs]) = MouseButtonUp (fromIntegral b) (fromIntegral bs) (fromIntegral qs)
    fromObj (ObjectArray [ObjectInt 2, ObjectInt b, ObjectInt bs, ObjectInt qs]) = MouseButtonDown (fromIntegral b) (fromIntegral bs) (fromIntegral qs)
    fromObj (ObjectArray [ObjectInt 3, ObjectInt x, ObjectInt y, ObjectInt dx, ObjectInt dy, ObjectInt bs, ObjectInt qs]) = MouseMove (fromIntegral x) (fromIntegral y) (fromIntegral dx) (fromIntegral dy) (fromIntegral bs) (fromIntegral qs)
    fromObj (ObjectArray [ObjectInt 4, ObjectInt w, ObjectInt bs, ObjectInt qs]) = MouseWheel (fromIntegral w) (fromIntegral bs) (fromIntegral qs)
    fromObj (ObjectArray [ObjectInt 5, ObjectBool v]) = MouseVisibleChanged v
    
ctMouseEvent :: ComponentType MouseEvent
ctMouseEvent = ComponentType 0x27eaf3fd46595d08

