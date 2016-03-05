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

      hg3d <- configureHG3D      -- initialize

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
            ctText #: " Exit",
            ctScreenRect #: Rectangle 200 10 50 25
            ]

      registerCallback hg3d eButton ctButton (\flag -> if not flag then exitHG3D hg3d else return ())

      -- create cube
      eGeo <- newE [
            ctGeometry #: ShapeGeometry Cube,
            ctMaterial #: matBlue,
            ctScale #: Vec3 10.0 10.0 10.0,
            ctPosition #: Vec3 0.0 0.0 0.0,
            ctOrientation #: unitU
            ]

      return (eGeo, eCam, hg3d)

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


main = do 
      (eGeo, eCam, hg3d) <- start
      forkIO $ rotateZ eGeo
      forkIO $ rotateX eGeo
      loopHG3D hg3d (msecT 20)
      return ()
