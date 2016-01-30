{-
    Sample: LiveStart, motivational example (www.hgamer3d.org/LiveSession.html)
	HGamer3D Library (A project to enable 3D game development in Haskell)
	Copyright 2011-2015 Peter Althainz
	
	Distributed under the Apache License, Version 2.0
	(See attached file LICENSE or copy at 
	http://www.apache.org/licenses/LICENSE-2.0)
 
	file: Samples/LiveStarter.hs
-}

{-# LANGUAGE OverloadedStrings #-}


{-

-- CH3-5s
:l LiveStarter
(w, c, g, l) <- main
-- CH3-5e

experiment with those:

-- CH3-6s
t <- forkIO $ forever $ updateC g ctOrientation (\ori -> (rotU vec3Y 0.05) .*. ori) >> sleepFor (msecT 50)
killThread t

setC l ctLight (Light (SpotLight (Deg 50) 1.0) 1.0 1000.0 1.5)
setC l ctColour white

setC g ctPosition (Vec3 0 0 (10.0))
setC g ctScale (Vec3 5.0 5.0 1.0)
setC c ctPosition (Vec3 0 0 0)

setC g ctMaterial matMetal
setC g ctGeometry (ShapeGeometry Cylinder)
-- CH3-6e

-}

-- CH3-1s
module Main where

import HGamer3D

import qualified Data.Text as T
import Control.Concurrent
import Control.Monad
-- CH3-1e

-- routine to initialize the system
-- CH3-2s
startWorld = do
    eG3D <- newE [
        ctGraphics3DConfig #: standardGraphics3DConfig,
        ctGraphics3DCommand #: NoCmd
        ]
    -- create G3D System
    world <- forkGraphics3DWorld (setC eG3D ctGraphics3DCommand Step >> return False) (msecT 20)
    addToWorld world eG3D
    return world
-- CH3-2e

-- CH3-3s
-- small tool, to create entities
creator w l = do
                e <- newE l
                addToWorld w e
                return e

-- entity creation tools
camera w pos = creator w [
                ctCamera #: FullViewCamera,
                ctPosition #: pos
                ]
    
cube w pos = creator w [
                ctMaterial #: matBlue,
                ctGeometry #: ShapeGeometry Cube,
                ctPosition #: pos,
                ctScale #: unitVec3,
                ctOrientation #: unitU
                ]
    
light w pos = creator w [
                ctLight #: Light PointLight 1.0 1000.0 1.0,
                ctPosition #: pos,
                ctColour #: white
                ]
-- CH3-3e

-- the main program, putting all together        
-- CH3-4s
main = do
    w <- startWorld
    c <- camera w (Vec3 0.0 0.0 0.0)
    g <- cube w (Vec3 0.0 0.0 (10.0))
    l <- light w (Vec3 0.0 0.0 0.0)
    return (w, c, g, l)
-- CH3-4e

    
