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

gameLogic hg3d = do

      -- create camera
      eCam <- newE hg3d [
            ctCamera #: FullViewCamera,
            ctPosition #: Vec3 1 1 (-30.0),
            ctLight #: Light PointLight 1.0 1000.0 1.0 
            ]

      eText <- newE hg3d [
            ctText #: "Rotating Cube Example",
            ctScreenRect #: Rectangle 10 10 100 25
            ]

      -- CH5-1s
      eButton <- newE hg3d [
            ctButton #: False,
            ctText #: " Exit",
            ctScreenRect #: Rectangle 200 10 50 25
            ]

      registerCallback hg3d eButton ctButton (\flag -> if not flag then exitHG3D hg3d else return ())
      -- CH5-1e

      -- create cube
      -- CH4-1s
      eGeo <- newE hg3d [
            ctGeometry #: ShapeGeometry Cube,
            ctMaterial #: matBlue,
            ctScale #: Vec3 10.0 10.0 10.0,
            ctPosition #: Vec3 0.0 0.0 0.0,
            ctOrientation #: unitU
            ]
      -- CH4-1e


      -- rotate the cube
      let rotateZ eGeo = do
            forever $ do 
                  updateC eGeo ctOrientation (\u -> (rotU vec3Z 0.021) .*. u)
                  sleepFor (msecT 12)
            return ()

      -- CH4-2s
      let rotateX eGeo = do
            forever $ do 
                  updateC eGeo ctOrientation (\u -> (rotU vec3X 0.012) .*. u)
                  sleepFor (msecT 16)
            return ()
-- CH4-2e


      forkIO $ rotateZ eGeo

      forkIO $ rotateX eGeo



      return ()


main = do 
      runGame standardGraphics3DConfig gameLogic (msecT 20)
      return ()
