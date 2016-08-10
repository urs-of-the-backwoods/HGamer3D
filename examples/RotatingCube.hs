{-# LANGUAGE OverloadedStrings #-}
module Main where

import HGamer3D

import qualified Data.Text as T
import Control.Concurrent
import Control.Monad
import System.Exit

gameLogic hg3d = do

    -- create minimum elements, like a camera
    eCam <- newE hg3d [
        ctCamera #: FullViewCamera,
        ctPosition #: Vec3 1 1 (-30.0),
        ctLight #: Light PointLight 1.0 1000.0 1.0 
        ]

    -- do something interesting here, in this example case, it is a text and
    -- a rotating cube

    eText <- newE hg3d [
        ctText #: "Rotating Cube Example",
        ctScreenRect #: Rectangle 10 10 100 25
        ]

    eGeo <- newE hg3d [
        ctGeometry #: ShapeGeometry Cube,
        ctMaterial #: matBlue,
        ctScale #: Vec3 10.0 10.0 10.0,
        ctPosition #: Vec3 0.0 0.0 0.0,
        ctOrientation #: unitU
        ]

    let rotateCube = do
            forever $ do 
                updateC eGeo ctOrientation (\u -> (rotU vec3Z 0.02) .*. u)
                updateC eGeo ctOrientation (\u -> (rotU vec3X 0.015) .*. u)
                sleepFor (msecT 12)

    forkIO rotateCube
    return ()

main = do 
    runGame standardGraphics3DConfig gameLogic (msecT 20)
    return ()
