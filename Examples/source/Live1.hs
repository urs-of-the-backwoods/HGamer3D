module Main  where

import HGamer3D.Data
import HGamer3D.Common
import HGamer3D.Graphics3D

import Control.Concurrent

-- START SYSTEM
---------------

-- run graphics system
go = forkGraphics3DWorld (msecT 30)

-- define a camera, lights, events and env entity
camera1 = Camera (Frustum 5.0 5000.0 (Deg 40)) (Viewport 0 (Rectangle 0.0 0.0 1.0 1.0) black)
light1 = Light white white PointLight 

env world = do
  envE <- newE [
       CTCam #: camera1,
       CTScP #: SceneParameter white NoShadows NoSky
       ]
  liE1 <- newE [
       CTLig #: light1,
       CTPos #: Vec3 500.0 500.0 (-500.0)
       ]
  liE2 <- newE [
       CTLig #: light1,
       CTPos #: Vec3 100.0 50.0 0.0	
       ]
  mapM (addToWorld world) [envE, liE1, liE2]  
  return envE


-- CONTENT CREATION

-- add entities to running system
addE world e = addToWorld world e

-- sized figure
sizedF geo scale = CombinedFigure [(zeroVec3, unitU, unitVec3 &* scale, geo)]

-- some geos
dodRed = SimpleFigure Dodekaeder (ResourceMaterial "Colours/Red")
cubeGreen = SimpleFigure Cube (ResourceMaterial "Colours/Green")
ikoBlue = SimpleFigure Ikosaeder (ResourceMaterial "Colours/Blue")

-- create geometry with size
newF fig scale = do
  geoE <- newE [
     CTPos #: Vec3 0.0 0.0 (-20.0),
     CTOri #: unitU,
     CTFig #: (sizedF fig scale)
     ]
  return geoE

-- CONTENT MODIFICATION
-----------------------

-- modify some properties of entities
x e l = updateE e  CTPos (\(Vec3 x y z) -> (Vec3 (x + l) y z))
y e l = updateE e CTPos (\(Vec3 x y z) -> (Vec3 x (y + l) z))
g e geo = updateE e CTFig (const geo)
rx e a = updateE e CTOri (\ori -> ori `multU` (rotU vec3X a))
ry e a = updateE e CTOri (\ori -> ori `multU` (rotU vec3Y a))

-- add some loop functions
start f sl = forkIO (let f' = f >> sleepFor sl >> f' in f')
stop tid = killThread tid

-- figure switcher
f2 g1 g2 = g g1 (sizedF dodRed 1.0) >> g g2 (sizedF cubeGreen 0.02)
sw g1 g2 sl = forkIO (let f' = f2 g1 g2 >> sleepFor sl >> f2 g2 g1 >> sleepFor sl >> f' in f')

example1 = do

  {-
    You can try the single commands below also separately in the ghci interpreter.
    Fire up emacs, load this small script, then type C-c C-l to load it in 
    inferior mode (ghci shell in Emacs). Then try the single commands in
    the emacs ghci window!
  -}

  -- start the system
  world <- go
  eE <- env world

  -- create some geometries
  g1 <- newF dodRed 1.0
  addE world g1
  g2 <- newF cubeGreen 0.02
  addE world g2
  x g2 4.0
  g3 <- newF ikoBlue 1.8
  addE world g3
  x g3 (-4.0)

  -- start some movement
  t1 <- start (rx g1 0.02) (msecT 30)
  t2 <- start (ry g2 0.02) (msecT 10)
  t3 <- start (rx g3 0.01 >> ry g3 0.02) (msecT 20)
  t4 <- sw g1 g2 (secT 5)

  return (world, eE, g1, g2, g3, t1, t2, t3, t4)

  {- 

  If you do not want to go step by step, start example with:

  (world, eE, g1, g2, g3, t1, t2, t3, t4) <- example1

  then you can stop the simple movements with 
  stop t1
  stop t3
  ...
  stop t4

  and use the entities g1 - g3 for experimenting

  g g3 (sizedF (SimpleFigure Sphere (ResourceMaterial "Floor/Steel2")) 0.02)
  x g3 (-20.0)

  -}


-- we also can compile into executable
main = do
  (world, eE, g1, g2, g3, t1, t2, t3, t4) <- example1
  qvar <- regQuitHandler eE
  takeMVar qvar

