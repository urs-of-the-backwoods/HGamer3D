{-# LANGUAGE OverloadedStrings #-}

module Main where

import HGamer3D

import qualified Data.Text as T
import Control.Concurrent
import Control.Monad

createAll hg3d = do

    -- create all elements
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
            
            ,[
                ctMouseConfig #: MouseConfig Absolute
            ]

            ,[
                ctInputEventHandler #: DefaultEventHandler, 
                ctKeyEvent #: NoKeyEvent,
                ctMouseEvent #: NoMouseEvent
            ]

            ,[
                ctEditText #: "Keys: A - Absolute Mouse Mode, R - Relative Mouse Mode, W - Wrap Mouse Mode",
                ctScreenRect #: ScreenRect 10 10 200 10
            ]

            ,[
                ctEditText #: "Mouse Mode Set To ...",
                ctScreenRect #: ScreenRect 10 25 200 10
            ]

            ,[
                ctEditText #: "Mouse Event",
                ctScreenRect #: ScreenRect 10 40 200 10
            ]
            
        ]
    let [cam, geo, mode, event, _, txtMode, txtEvent] = res
    return (geo, mode, event, txtMode, txtEvent)


showMode mode txtMode = do
    forever $ do 
        m <- readC mode ctMouseConfig
        setC txtMode ctEditText (T.pack (show m))
        sleepFor (msecT 20)
    return ()

addMouseEventCallback hg3d event txtEvent = registerCallback hg3d event ctMouseEvent (\evt -> setC txtEvent ctEditText (T.pack (show evt))) 
addKeyEventCallback hg3d event mode = registerCallback hg3d event ctKeyEvent (\evt -> case evt of
                                                                                            KeyUpEvent (KeyData _ _ "A") -> setC mode ctMouseConfig (MouseConfig Absolute)
                                                                                            KeyUpEvent (KeyData _ _ "R") -> setC mode ctMouseConfig (MouseConfig Relative)
                                                                                            KeyUpEvent (KeyData _ _ "W") -> setC mode ctMouseConfig (MouseConfig Wrap) 
                                                                                            _ -> return ())

rotateCube cube = do
    forever $ do 
        updateC cube ctOrientation (\u -> (rotU vec3Z 0.021) .*. u)
        updateC cube ctOrientation (\u -> (rotU vec3X 0.012) .*. u)
        sleepFor (msecT 30)
    return ()


gameLogic hg3d = do
    (cube, mode, event, txtMode, txtEvent) <- createAll hg3d
    forkIO $ showMode mode txtMode
    forkIO $ rotateCube cube
    addMouseEventCallback hg3d event txtEvent
    addKeyEventCallback hg3d event mode
    return ()

main = do
    runGame standardGraphics3DConfig gameLogic (msecT 20)
    return ()


