{-# LANGUAGE OverloadedStrings #-}

module Main where

import HGamer3D

import qualified Data.Text as T
import Control.Concurrent
import Control.Monad

start = do

      eG3D <- newE [
            ctGraphics3DConfig #: standardGraphics3DConfig,
            ctGraphics3DCommand #: NoCmd
            ]

      -- create G3D System
      world <- forkGraphics3DWorld (setC eG3D ctGraphics3DCommand Step >> return False) (msecT 20)
      addToWorld world eG3D

      -- create camera
      eCam <- newE [
            ctCamera #: MainCamera,
            ctPosition #: Vec3 1 1 (-30.0),
            ctLight #: PointLight (LightParameter 1.0 1000.0 1.0 False)
            ]

      addToWorld world eCam

      -- create Geometry
      eGeo <- newE [
            ctGeometry #: ShapeGeometry Cube,
            ctMaterial #: matOrangeCrossMetal,
            ctScale #: Vec3 10.0 10.0 10.0,
            ctPosition #: Vec3 0.0 0.0 0.0,
            ctOrientation #: unitU
            ]
      addToWorld world eGeo

      return (eGeo, eCam)

rotate eGeo = do
      forever $ do 
            updateC eGeo ctOrientation (\u -> (rotU vec3Y 0.02) .*. u)
            sleepFor (msecT 20)
      return ()

main = do
      (eGeo, eCam) <- start
      rotate eGeo
      return ()
