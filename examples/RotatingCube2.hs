{-# LANGUAGE OverloadedStrings #-}

module Main where

import HGamer3D
import Control.Concurrent
import Control.Monad

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
                  ctScale #: Vec3 5.0 5.0 5.0,
                  ctPosition #: Vec3 0.0 0.0 0.0,
                  ctOrientation #: unitU
                  ], [
            -- CH4-1e
                        "eSmall" <| ([
                              ctGeometry #: ShapeGeometry Sphere,
                              ctMaterial #: matGreen,
--                              ctGraphicsElement #: (),
                              ctPosition #: Vec3 (-0.5) 0.5 (-0.5),
                              ctScale #: Vec3 0.8 0.8 0.8,
                              ctOrientation #: unitU
                              ],
                              [
                                () -: [
                                    ctGeometry #: ShapeGeometry Sphere,
                                    ctMaterial #: matRed,
                                    ctScale #: Vec3 0.5 0.5 0.5,
                                    ctPosition #: Vec3 (0.0) (-0.5) (-0.5),
                                    ctOrientation #: unitU
                                    ]
                              ])
                  ])
            ]

      registerCallback hg3d (es # "eButton") ctButton (\(Button flag _) -> if not flag then exitHG3D hg3d else return ())

      -- rotate the cube
      let rotate = do
            forever $ do 
                  updateC (es # "eGeo") ctOrientation (\u -> (rotU vec3X 0.0013) .*. u)
                  updateC (es # "eGeo") ctOrientation (\u -> (rotU vec3Y 0.005) .*. u)
                  updateC (es # "eSmall") ctOrientation (\u -> (rotU (Vec3 (-1.0) 1.0 (-1.0)) 0.05) .*. u)
                  sleepFor (msecT 20)
            return ()

-- CH4-2e
      forkIO $ rotate
      return ()

main = do 
      runGame standardGraphics3DConfig gameLogic (msecT 20)
      return ()
