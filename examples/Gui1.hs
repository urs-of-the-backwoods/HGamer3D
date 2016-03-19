{-# LANGUAGE OverloadedStrings #-}

module Main where

import HGamer3D

import qualified Data.Text as T
import Control.Concurrent
import Control.Monad

createAll = do
    res <- mapM newE [
            -- graphics system creation
            [
                ctGraphics3DConfig #: standardGraphics3DConfig,
                ctGraphics3DCommand #: NoCmd
            ],

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
                ctText #: "A Button: \n"
                , ctScreenRect #: Rectangle 10 10 120 25
            ]
           ,[   -- button
                ctButton #: False
                , ctText #: "Press Me"
                , ctScreenRect #: Rectangle 130 10 100 25
            ]
           ,[   -- descriptive text
                ctText #: "A Checkbox: \n"
                , ctScreenRect #: Rectangle 10 40 120 25
            ]
           ,[   -- checkbox
                ctCheckBox #: False
                , ctScreenRect #: Rectangle 130 40 15 15
            ]
           ,[   -- descriptive text
                ctText #: "An EditText: \n"
                , ctScreenRect #: Rectangle 10 70 120 25
            ]
           ,[   -- edittext
                ctEditText #: ""
                , ctScreenRect #: Rectangle 130 70 150 25
            ]
           ,[   -- descriptive text
                ctText #: "A Slider: \n"
                , ctScreenRect #: Rectangle 10 100 120 25
            ]
           ,[   -- slider
                ctSlider #: Slider 100 10
                , ctScreenRect #: Rectangle 130 100 150 25
            ]
-- CH6-1s    
           ,[   -- descriptive text
                ctText #: "A DropDownList: \n"
                , ctScreenRect #: Rectangle 10 130 120 25
            ]
           ,[   -- dropdownlist
                ctDropDownList #: DropDownList ["hi", "you"] Nothing
                , ctScreenRect #: Rectangle 130 130 150 25
            ]
-- CH6-1e
           ,[   -- output text
                ctText #: "Output Events\n"
                , ctScreenRect #: Rectangle 350 10 300 400
            ]
        ]

    print "gui1 - 1"
    -- create callback for gui
    forkIO $ do
        cbs <- createCBS
        forever (stepCBS cbs)

    return res

rotate eGeo eG3D = do
    print "gui1 - 4"
    forever $ do 
        updateC eGeo ctOrientation (\u -> (rotU vec3Y 0.02) .*. (rotU vec3X 0.005) .*. u)
        setC eG3D ctGraphics3DCommand Step
        sleepFor (msecT 20)
    return ()

-- CH6-2s
printEvents button checkbox edittext slider dropdownlist output = do
    forever $ do
        textOut <- do
                     t1 <- readC button ctButton
                     t2 <- readC checkbox ctCheckBox
                     t3 <- readC edittext ctEditText
                     t4 <- readC slider ctSlider
                     t5 <- readC dropdownlist ctDropDownList
                     return (T.pack ("button: " ++ (show t1) ++ "\ncheckbox: " ++ (show t2) ++ "\nedittext: " ++ (show t3) ++ "\nslider: " ++ (show t4) ++ "\ndropdownlist: " ++ (show t5) ++ "\n\n"))
        setC output ctText textOut
        sleepFor (msecT 200)
        return ()
-- CH6-2e

main = do
    print "gui1 - 0"
    [eg3d, camera, cube, _, button, _, checkbox, _, edittext, _, slider, _, dropdownlist, output] <- createAll
    print "gui1 - 2"
    forkIO $ printEvents button checkbox edittext slider dropdownlist output
    print "gui1 - 3"
    rotate cube eg3d
    return ()
