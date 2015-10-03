{-
    Sample: engineShutdown, test, if engine shuts down properly
	HGamer3D Library (A project to enable 3D game development in Haskell)
	Copyright 2011-2015 Peter Althainz
	
	Distributed under the Apache License, Version 2.0
	(See attached file LICENSE or copy at 
	http://www.apache.org/licenses/LICENSE-2.0)
 
	file: Samples/engineShutdown.hs
-}

{-# LANGUAGE OverloadedStrings #-}

module Main where

import HGamer3D

import qualified Data.Text as T
import Control.Concurrent
import Control.Monad

start = do


      -- create G3D System
      eG3D <- newE [
            ctGraphics3DConfig #: standardGraphics3DConfig,
            ctGraphics3DCommand #: NoCmd
            ]
      world <- forkGraphics3DWorld (setC eG3D ctGraphics3DCommand Step >> return False) (msecT 20)

      -- wait until everything is initialized
      print "after press, system will initialize" 
      l <- getLine
      addToWorld world eG3D

      print "after press, system will shutdown"
      l <- getLine
      removeFromWorld world eG3D

      print "after press, system will initialize again"
      l <- getLine
      addToWorld world eG3D


      print "after press, graphics will initialize"
      l <- getLine

      -- create camera
      eCam <- newE [ 
            ctCamera #: FullViewCamera ,
            ctPosition #: Vec3 0 0 (-20.0),
            ctLight #:  Light PointLight 1.0 1000.0 1.0 
            ]
      addToWorld world eCam

      -- create Geometry
      eGeo <- newE [
            ctGeometry #: ShapeGeometry Cube,
            ctMaterial #: ResourceMaterial "Materials/Stone.xml",
            ctScale #: Vec3 2.0 2.0 2.0,
            ctPosition #: Vec3 0.0 0.0 0.0,
            ctOrientation #: unitU
            ]
      addToWorld world eGeo

      return eGeo

loop eGeo = forever $ do 
            updateC eGeo ctOrientation (\u -> (rotU vec3Y 0.02) .*. u)
            sleepFor (msecT 20)

demo = do
      eGeo <- start
      forkIO (loop eGeo)

main = do
      eGeo <- start
      loop eGeo
