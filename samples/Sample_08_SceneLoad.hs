{-# LANGUAGE OverloadedStrings #-}

module Sample_08_SceneLoad where

import HGamer3D
import Control.Concurrent
import Control.Monad
import SampleRunner

creator hg3d = do

    eScene <- newE hg3d [
      ctScene #: XmlScene "Scenes/SceneLoadExample.xml",
      ctScale #: Vec3 4.0 4.0 4.0,
      ctPosition #: Vec3 (0.0) (-3.0) (0.0)

        ]

    sky <- newE hg3d [
      ctSkybox #: SkyboxMaterial "Materials/Skybox.xml"
                    ]

    return (eScene, sky)

destructor (eScene, sky) = do
  delE eScene
  delE sky
  return ()

sampleRunner hg3d = SampleRunner (return ()) (do
                                    state <- creator hg3d
                                    return (SampleRunner (destructor state) (return emptySampleRunner) ))

