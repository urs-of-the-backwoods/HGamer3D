{-# LANGUAGE OverloadedStrings #-}
{-
:l RotatingCube
main
-}

module Main where

import HGamer3D

import qualified Data.Text as T
import qualified Data.ByteString as B
import Control.Concurrent
import Control.Monad
import System.Exit

import qualified Data.Map as M
import Data.Maybe

gameLogic hg3d = do

      es <- newET hg3d [
            -- create camera
            () -: [
                  ctCamera #: FullViewCamera,
                  ctPosition #: Vec3 1 1 (-30.0),
                  ctLight #: Light PointLight 1.0 1000.0 1.0 
                  ],

            () -: [
                  ctText #: "Rotating Cube Example",
                  ctScreenRect #: Rectangle 10 10 100 25
                  ],

            -- CH5-1s
            "eButton" <: [
                  ctButton #: Button False "Exit",
                  ctScreenRect #: Rectangle 200 10 50 25
                  ],

            -- CH5-1e

            -- create cube and sphere
            -- CH4-1s
            "eGeo" <| ([
                  ctGeometry #: ShapeGeometry Cube,
                  ctMaterial #: matBlue,
                  ctScale #: Vec3 10.0 10.0 10.0,
                  ctPosition #: Vec3 0.0 0.0 0.0,
                  ctOrientation #: unitU
                  ], [
            -- CH4-1e
                        () -: [
                              ctGeometry #: ShapeGeometry Sphere,
                              ctMaterial #: matRed,
                              ctScale #: Vec3 1.0 1.0 1.0,
                              ctPosition #: Vec3 0.0 13.0 0.0,
                              ctOrientation #: unitU
                              ]
                  ])
            ]

      registerCallback hg3d (es # "eButton") ctButton (\(Button flag _) -> if not flag then exitHG3D hg3d else return ())

      let eGeo = es # "eGeo"

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
