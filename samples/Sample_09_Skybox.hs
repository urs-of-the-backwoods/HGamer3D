{-# LANGUAGE OverloadedStrings #-}

module Sample_09_Skybox where

import HGamer3D
import Control.Concurrent
import Control.Monad
import SampleRunner

camy c d = do
     updateC c ctOrientation (\u -> (rotU vec3Y d) .*. u)
     updateC c ctPosition (\p -> rotate3 d vec3Y p)

rotateWorld c quitV =  do
                          camy c 0.002
                          sleepFor (msecT 20)
                          q <- readVar quitV
                          if not q
                            then rotateWorld c quitV
                            else return ()

creator hg3d c = do

    eGeo <- newE hg3d [
        ctGeometry #: ShapeGeometry Cube,
        ctMaterial #: matWoodTiles,
        ctScale #: Vec3 8.0 8.0 8.0,
        ctPosition #: Vec3 0.0 0.0 0.0,
        ctOrientation #: unitU
        ]

    sky <- newE hg3d [
      ctSkybox #: SkyboxMaterial "Materials/Skybox.xml"
                    ]

    quitV <- makeVar False

    let rotateCube = do
                updateC eGeo ctOrientation (\u -> (rotU vec3Z 0.02) .*. u)
                sleepFor (msecT 12)
                q <- readVar quitV
                if not q
                  then rotateCube
                  else return ()

    forkIO rotateCube
    forkIO $ rotateWorld c quitV

    return (c, eGeo, sky, quitV)

destructor (c, eGeo, sky, quitV) = do
  writeVar quitV True
  sleepFor (msecT 500) -- monitor that cube stops before deletion
  delE eGeo
  delE sky
  setC c ctOrientation unitU
  setC c ctPosition (Vec3 1.0 1.0 (-30.0))
  return ()

sampleRunner hg3d c = SampleRunner (return ()) (do
                                    state <- creator hg3d c
                                    return (SampleRunner (destructor state) (return emptySampleRunner) ))

