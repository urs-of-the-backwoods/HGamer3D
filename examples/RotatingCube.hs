{-# LANGUAGE OverloadedStrings #-}
{-
:l RotatingCube
main
-}

module Main where

import HGamer3D

import qualified Data.Text as T
import Control.Concurrent
import Control.Monad
import System.Exit

start = do

      -- create graphics system
      eG3D <- newE [
            ctGraphics3DConfig #: standardGraphics3DConfig,
            ctGraphics3DCommand #: NoCmd
            ]

      -- create camera
      eCam <- newE [
            ctCamera #: FullViewCamera,
            ctPosition #: Vec3 1 1 (-30.0),
            ctLight #: Light PointLight 1.0 1000.0 1.0 
            ]

      -- create GUI
      eText <- newE [
            ctText #: "Rotating Cube Example",
            ctScreenRect #: Rectangle 10 10 100 25
            ]

      eButton <- newE [
            ctButton #: False,
            ctText #: " Raus Hier",
            ctScreenRect #: Rectangle 200 10 50 25
            ]

      -- create callback for gui
      forkIO $ do
            cbs <- createCBS
            registerReceiverCBS cbs eButton ctButton (\flag -> if (not flag) then exitSuccess else return ())
            forever (stepCBS cbs)

      -- create cube
      eGeo <- newE [
            ctGeometry #: ShapeGeometry Cube,
            ctMaterial #: matBlue,
            ctScale #: Vec3 10.0 10.0 10.0,
            ctPosition #: Vec3 0.0 0.0 0.0,
            ctOrientation #: unitU
            ]

      return (eGeo, eCam, eG3D)

-- rotate the cube
rotateZ eGeo = do
      forever $ do 
            updateC eGeo ctOrientation (\u -> (rotU vec3Z 0.021) .*. u)
            sleepFor (msecT 12)
      return ()

rotateX eGeo = do
      forever $ do 
            updateC eGeo ctOrientation (\u -> (rotU vec3X 0.012) .*. u)
            sleepFor (msecT 16)
      return ()

-- show graphics
showPic eG3D = do
      forever $ do
            setC eG3D ctGraphics3DCommand Step
            sleepFor (msecT 2)
      return ()

main = do 
      print $ "rotating main"

      (eGeo, eCam, eG3D) <- start
      forkIO $ rotateZ eGeo
      forkIO $ rotateX eGeo
      forkIO $ showPic eG3D
      forever $ sleepFor (secT 1)
      return ()
