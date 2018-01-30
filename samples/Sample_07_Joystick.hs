{-# LANGUAGE OverloadedStrings #-}

module Main where

import HGamer3D

import qualified Data.Text as T
import Control.Concurrent
import Control.Monad

createEntities hg3d = do
    res <- mapM (newE hg3d) [
            [   -- camera
                ctCamera #: FullViewCamera,
                ctPosition #: Vec3 1 1 (-30.0),
                ctLight #: Light PointLight 1.0 1000.0 1.0 
            ]
           ,[   -- blue cube
                ctGeometry #: ShapeGeometry Cube,
                ctMaterial #: matBlue,
                ctScale #: Vec3 5.0 5.0 5.0,
                ctPosition #: Vec3 0.0 0.0 0.0,
                ctOrientation #: unitU
            ]
           ,[   -- descriptive text
                ctStaticText #: "A Button: \n"
                , ctScreenRect #: ScreenRect 10 10 120 25
            ]
           ,[   -- button
                ctButton #: Button False "Press Me"
                , ctScreenRect #: ScreenRect 130 10 100 25
            ]
           ,[   -- descriptive text
                ctStaticText #: "A Checkbox: \n"
                , ctScreenRect #: ScreenRect 10 40 120 25
            ]
           ,[   -- checkbox
                ctCheckBox #: False
                , ctScreenRect #: ScreenRect 130 40 15 15
            ]
           ,[   -- descriptive text
                ctStaticText #: "An EditText: \n"
                , ctScreenRect #: ScreenRect 10 70 120 25
            ]
           ,[   -- edittext
                ctEditText #: ""
                , ctScreenRect #: ScreenRect 130 70 150 25
            ]
           ,[   -- descriptive text
                ctStaticText #: "A Slider: \n"
                , ctScreenRect #: ScreenRect 10 100 120 25
            ]
           ,[   -- slider
                ctSlider #: Slider 100 10
                , ctScreenRect #: ScreenRect 130 100 150 25
            ]
-- CH6-1s    
           ,[   -- descriptive text
                ctStaticText #: "A DropDownList: \n"
                , ctScreenRect #: ScreenRect 10 130 120 25
            ]
           ,[   -- dropdownlist
                ctDropDownList #: DropDownList ["hi", "you"] NoSelection
                , ctScreenRect #: ScreenRect 130 130 150 25
            ]
-- CH6-1e
           ,[   -- output text
                ctStaticText #: "Output Events\n"
                , ctScreenRect #: ScreenRect 350 10 300 400
            ]

           ,[
               -- joystick 1
               ctJoystick #: Joystick 0
               ,ctJoystickEvent #: NoJoystickEvent
            ]
        ]

    let [camera, cube, _, button, _, checkbox, _, edittext, _, slider, _, dropdownlist, output, joystick] = res
    return (camera, cube, button, checkbox, edittext, slider, dropdownlist, output, joystick)


startRotation cube = do
    let rotate = do
            updateC cube ctOrientation (\u -> (rotU vec3Y 0.02) .*. (rotU vec3X 0.005) .*. u)
            sleepFor (msecT 20)
            rotate 
    forkIO $ rotate


startPrintEvents button checkbox edittext slider dropdownlist output joystick = do
    let printEvents = do
            forever $ do
                textOut <- do
                             t1 <- readC joystick ctJoystickEvent
                             t2 <- readC checkbox ctCheckBox
                             t3 <- readC edittext ctEditText
                             t4 <- readC slider ctSlider
                             t5 <- readC dropdownlist ctDropDownList
                             return (T.pack ("button: " ++ (show t1) ++ "\ncheckbox: " ++ (show t2) ++ "\nedittext: " ++ (show t3) ++ "\nslider: " ++ (show t4) ++ "\ndropdownlist: " ++ (show t5) ++ "\n\n"))
                setC output ctStaticText textOut
                sleepFor (msecT 200)
                return ()
    forkIO $ printEvents


gameLogic hg3d = do
    (camera, cube, button, checkbox, edittext, slider, dropdownlist, output, joystick) <- createEntities hg3d
    startRotation cube
    startPrintEvents button checkbox edittext slider dropdownlist output joystick
    return ()

main = do
    runGame standardGraphics3DConfig gameLogic (msecT 20)
    return ()
