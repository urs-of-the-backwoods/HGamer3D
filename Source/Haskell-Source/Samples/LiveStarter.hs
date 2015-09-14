{-# LANGUAGE OverloadedStrings #-}

{-

This short introduction defines some functions to create the main entities.
Below is a comment section, which contains commands.

download windows toolset here: http://www.hgamer3d.org/downloads/HG3D-Toolset-Windows.zip
copy content in a directory (example: c:\HG3DTools), add this directory to your path
start Cmder from this directory
install HGamer3D (see www.hgamer3d.org)
run stack init
run notepad++

^R runs a text selection (or current line if nothing is selected)
^D defines a function in a text selection (with let in the beginnign for GHCI)
^. switches between output and this text
^<del> stops background process

Usable command lines:

:s OverloadedStrings
:l Test
(w, c, g, l) <- main

setC l ctLight (Light (SpotLight (Deg 50) 1.0) 1.0 1000.0 1.5)
setC l ctColour white

setC g ctPosition (Vec3 0 0 (10.0))
setC g ctScale (Vec3 5.0 5.0 1.0)
setC c ctPosition (Vec3 0 0 0)

setC g ctMaterial matMetal
setC g ctGeometry (ShapeGeometry Cylinder)

-}

module Main where

import HGamer3D

import qualified Data.Text as T
import Control.Concurrent
import Control.Monad

startWorld = do
    eG3D <- newE [
        ctGraphics3DConfig #: standardGraphics3DConfig,
        ctGraphics3DCommand #: NoCmd
        ]
    -- create G3D System
    world <- forkGraphics3DWorld (setC eG3D ctGraphics3DCommand Step >> return False) (msecT 20)
    addToWorld world eG3D
    return world

creator w l = do
                e <- newE l
                addToWorld w e
                return e
    
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
                
mouse w = creator w [
            ctMouse #: Mouse MMAbsolute,
            ctMouseEvent #: undefined,
            ctVisible #: False
            ]
            
keyboard w = creator w [
            ctKeyEvent #: KeyUp 0 0 ""
            ]

            
testMouseModes m k = do
    let handleKeys n = do
                            case n of
                                KeyUp _ _ "A" -> setC m ctMouse (Mouse MMAbsolute)
                                KeyUp _ _ "R" -> setC m ctMouse (Mouse MMRelative)
                                KeyUp _ _ "W" -> setC m ctMouse (Mouse MMWrap)
                                KeyUp _ _ "V" -> setC m ctVisible True
                                _ -> return ()
                            return ()
        in addListener  k ctKeyEvent (\_ enew -> handleKeys (enew # ctKeyEvent))
            
main = do
    w <- startWorld
    c <- camera w (Vec3 0.0 0.0 0.0)
    g <- cube w (Vec3 0.0 0.0 (10.0))
    l <- light w (Vec3 0.0 0.0 0.0)
    return (w, c, g, l)

    
