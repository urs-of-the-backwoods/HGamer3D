{-# LANGUAGE OverloadedStrings #-}

module Sample_10_3DText where

import HGamer3D
import Control.Concurrent
import Control.Monad
import SampleRunner

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
                        "eSelector" <: [
                              ctDropDownList #: DropDownList ["None", "RotateXYZ", "RotateY", "LookatXYZ", "LookatY", "LookatMixed", "Direction" ] (Selection 1),
                              ctScreenRect #: ScreenRect 10 100 300 20
                              ],

                        "eSmall" <| ([
                              ctGeometry #: ShapeGeometry Sphere,
                              ctMaterial #: matGreen,
                              ctPosition #: Vec3 (-0.5) 0.5 (-0.5),
                              ctScale #: Vec3 0.8 0.8 0.8,
                              ctOrientation #: unitU
                              ],
                              [
                                () -| ([
                                    ctGeometry #: ShapeGeometry Sphere,
                                    ctMaterial #: matRed,
                                    ctScale #: Vec3 0.5 0.5 0.5,
                                    ctPosition #: Vec3 (0.0) (-0.5) (-0.5),
                                    ctOrientation #: unitU
                                    ],
                                    [
                                      "eText" <: [
                                        ctText3D #: Text3D "Fonts/BlueHighway.ttf" 24 FCRotateXYZ False,
                                        ctLabel #: "Red Sphere",
                                        ctScale #: Vec3 2.0 2.0 2.0,
                                        ctPosition #: Vec3 (0.5) (0.0) (0.5)
                                                 ]

                                    ])
                              ])
                  ])

            ]

    quitV <- makeVar False

    registerCallback hg3d (es # "eSelector") ctDropDownList (\(DropDownList _ selection) -> case selection of
                                                                  Selection t -> case t of
                                                                    0 -> setC (es # "eText") ctText3D (Text3D "Fonts/BlueHighway.ttf" 24 FCNone False)
                                                                    1 -> setC (es # "eText") ctText3D (Text3D "Fonts/BlueHighway.ttf" 24 FCRotateXYZ False)
                                                                    2 -> setC (es # "eText") ctText3D (Text3D "Fonts/BlueHighway.ttf" 24 FCRotateY False)
                                                                    3 -> setC (es # "eText") ctText3D (Text3D "Fonts/BlueHighway.ttf" 24 FCLookatXYZ False)
                                                                    4 -> setC (es # "eText") ctText3D (Text3D "Fonts/BlueHighway.ttf" 24 FCLookatY False)
                                                                    5 -> setC (es # "eText") ctText3D (Text3D "Fonts/BlueHighway.ttf" 24 FCLookatMixed False)
                                                                    6 -> setC (es # "eText") ctText3D (Text3D "Fonts/BlueHighway.ttf" 24 FCDirection False)
                                                                  NoSelection -> return () )

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
  sleepFor (msecT 500) -- monitor that movement stops before deletion
  delET es
  return ()

sampleRunner hg3d = SampleRunner (return ()) (do
                                    state <- creator hg3d
                                    return (SampleRunner (destructor state) (return emptySampleRunner) ))

