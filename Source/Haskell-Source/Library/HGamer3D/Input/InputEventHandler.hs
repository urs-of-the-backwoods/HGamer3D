{-
	Input Event handler and settings
	HGamer3D Library (A project to enable 3D game development in Haskell)
	Copyright 2015 Peter Althainz
	
	Distributed under the Apache License, Version 2.0
	(See attached file LICENSE or copy at 
	http://www.apache.org/licenses/LICENSE-2.0)
 
	file: HGamer3D/Input/Mouse.hs
-}

-- | Module providing settings for all input events (Mouse, Keyboard, Joystick)
module HGamer3D.Input.InputEventHandler
(
    InputEventType (..),
    InputEventHandler (..),
    ctInputEventHandler
)

where

import Data.MessagePack
import Debug.Trace
import Data.Text

import HGamer3D.Data

data InputEventType  =  IEMouseButtonUp
                        | IEMouseButtonDown
                        | IEMouseMove
                        | IEMouseButtonWheel
                        | IEMouseButtonVisibleChanged
                        | IEKeyUp
                        | IEKeyDown

                        deriving (Eq, Show)

instance ComponentClass InputEventType where

    toObj iet = case iet of
        IEMouseButtonUp -> ObjectInt 1
        IEMouseButtonDown -> ObjectInt 2
        IEMouseMove -> ObjectInt 3
        IEMouseButtonWheel -> ObjectInt 4
        IEMouseButtonVisibleChanged -> ObjectInt 5
        IEKeyUp -> ObjectInt 6
        IEKeyDown -> ObjectInt 7
    fromObj (ObjectInt n) = case n of
        1 -> IEMouseButtonUp
        2 -> IEMouseButtonDown
        3 -> IEMouseMove
        4 -> IEMouseButtonWheel
        5 -> IEMouseButtonVisibleChanged
        6 -> IEKeyUp
        7 -> IEKeyDown
        
data InputEventHandler = DefaultEventHandler
                         | SpecificEventHandler [InputEventType]

instance ComponentClass InputEventHandler where
    toObj DefaultEventHandler = ObjectArray [ObjectInt 0]
    toObj (SpecificEventHandler iets) = ObjectArray [ObjectInt 1, ObjectArray (Prelude.map toObj iets)]

    fromObj (ObjectArray [ObjectInt 0]) = DefaultEventHandler
    fromObj (ObjectArray [ObjectInt 1, (ObjectArray iets_os)]) = SpecificEventHandler (Prelude.map fromObj iets_os)
    
ctInputEventHandler :: ComponentType InputEventHandler
ctInputEventHandler = ComponentType 0xfc0edefcebcb5878

