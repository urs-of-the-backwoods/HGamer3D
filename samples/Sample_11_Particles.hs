{-# LANGUAGE OverloadedStrings #-}

module Sample_11_Particles where

import HGamer3D
import Control.Concurrent
import Control.Monad
import SampleRunner

creator hg3d = do

    eP1 <- newE hg3d [
        ctParticles #: ParticleEffectResource "Particle/Smoke.xml",
        ctPosition #: Vec3 (-5.0) (-2.0) (-15.0)
        ]

    eP2 <- newE hg3d [
        ctParticles #: ParticleEffectResource "Particle/Fire.xml",
        ctPosition #: Vec3 0.0 (-2.0) (-15.0)
        ]

    eP3 <- newE hg3d [
        ctParticles #: ParticleEffectResource "Particle/Disco.xml",
        ctPosition #: Vec3 5.0 (-2.0) (-15.0)
        ]

    return (eP1, eP2, eP3)

destructor (eP1, eP2, eP3) = do
  delE eP1
  delE eP2
  delE eP3
  return ()

sampleRunner hg3d = SampleRunner (return ()) (do
                                    state <- creator hg3d
                                    return (SampleRunner (destructor state) (return emptySampleRunner) ))

