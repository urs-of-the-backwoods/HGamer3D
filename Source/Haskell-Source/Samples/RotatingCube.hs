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

      eG3D <- newE [
            ctGraphics3DConfig #: standardGraphics3DConfig,
            ctGraphics3DCommand #: NoCmd
            ]

      -- create G3D System
      world <- forkGraphics3DWorld (setC eG3D ctGraphics3DCommand Step >> return False) (msecT 20)
      addToWorld world eG3D

      -- create camera
      eCam <- newE [
            ctCamera #: FullViewCamera,
            ctPosition #: Vec3 1 1 (-30.0),
            ctLight #: Light PointLight 1.0 1000.0 1.0 
            ]
      addToWorld world eCam

      -- create GUI
      eText <- newE [
            ctText #: "Rotating Cube Example",
            ctScreenRect #: Rectangle 10 10 100 25
            ]
      addToWorld world eText
      -- CH5-1s
      -- adds a button
      eButton <- newE [
            ctButton #: False,
            ctText #: "Exit",
            ctScreenRect #: Rectangle 200 10 50 25
            ]
      addToWorld world eButton
      -- reacts to the change of button value
      addListener eButton ctButton (\_ enew ->
        if ((enew #! ctButton) == False)
            then removeFromWorld world eG3D >> exitSuccess   -- need to find a better way to close
            else return () )
      -- CH5-1e

      -- CH4-1s
      -- create Geometry
      eGeo <- newE [
            ctGeometry #: ShapeGeometry Cube,
            ctMaterial #: matOrangeCrossMetal,
            ctScale #: Vec3 10.0 10.0 10.0,
            ctPosition #: Vec3 0.0 0.0 0.0,
            ctOrientation #: unitU
            ]
      addToWorld world eGeo
      -- CH4-1e

      return (eGeo, eCam)

-- CH4-2s      
rotate eGeo = do
      forever $ do 
            updateC eGeo ctOrientation (\u -> (rotU vec3Y 0.02) .*. u)
            sleepFor (msecT 20)
      return ()
-- CH4-2e

main = do
      (eGeo, eCam) <- start
      rotate eGeo
      return ()
