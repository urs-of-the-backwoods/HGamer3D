{-# LANGUAGE OverloadedStrings #-}

module Sample_03_Hierarchy where

import SampleRunner
import HGamer3D
import Control.Concurrent
import Control.Monad

creator hg3d = do

      es <- newET hg3d [
            -- create geometry, with child geometry items
            "eGeo" <| ([
                  ctGeometry #: ShapeGeometry Cube,
                  ctMaterial #: matBlue,
                  ctScale #: Vec3 5.0 5.0 5.0,
                  ctPosition #: Vec3 0.0 0.0 0.0,
                  ctOrientation #: unitU
                  ], [
                        "eSmall" <| ([
                              ctGeometry #: ShapeGeometry Sphere,
                              ctMaterial #: matGreen,
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

      quitV <- makeVar False

      -- rotate the cube
      let rotate = do
                  updateC (es # "eGeo") ctOrientation (\u -> (rotU vec3X 0.0013) .*. u)
                  updateC (es # "eGeo") ctOrientation (\u -> (rotU vec3Y 0.005) .*. u)
                  updateC (es # "eSmall") ctOrientation (\u -> (rotU (Vec3 (-1.0) 1.0 (-1.0)) 0.05) .*. u)
                  sleepFor (msecT 20)
                  q <- readVar quitV
                  if not q
                    then rotate
                    else return ()

      forkIO $ rotate

      return (es, quitV)

destructor (es, quitV) = do
  writeVar quitV True
  sleepFor (msecT 500)
  delET es 
  return ()

sampleRunner hg3d = SampleRunner (return ()) (do
                                    state <- creator hg3d
                                    return (SampleRunner (destructor state) (return emptySampleRunner) ))

