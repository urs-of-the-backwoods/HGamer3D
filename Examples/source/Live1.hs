module Main  where

-- import needed HGamer3D modules
import HGamer3D.Data
import HGamer3D.Common
import HGamer3D.Graphics3D

-- we will use concurrency for moving parts
import Control.Concurrent

-- book: define 3d environment
-- create basic 3D environment, camera and lights
create3DEnvironment = do

  let camera = Camera (Frustum 5.0 5000.0 (Deg 40)) (Viewport 0 (Rectangle 0.0 0.0 1.0 1.0) black)
  let light = Light white white PointLight 

  sceneE <- newE [
       CTCam #: camera,
       CTScP #: SceneParameter white NoShadows NoSky
       ]
  lightE1 <- newE [
       CTLig #: light,
       CTPos #: Vec3 500.0 500.0 (-500.0)
       ]
  lightE2 <- newE [
       CTLig #: light,
       CTPos #: Vec3 100.0 50.0 0.0	
       ]

  return (sceneE, lightE1, lightE2)
-- book

-- book: create figures
-- some example figures
dodRed = SimpleFigure Dodekaeder (ResourceMaterial "Colours/Red")
cubeGreen = SimpleFigure Cube (ResourceMaterial "Colours/Green")
ikoBlue = SimpleFigure Ikosaeder (ResourceMaterial "Colours/Blue")

-- create figure with size
sizedF fig' scale' = CombinedFigure [(zeroVec3, unitU, unitVec3 &* scale', fig')]

newF fig scale = do
  figureE <- newE [
     CTPos #: Vec3 0.0 0.0 (-20.0),
     CTOri #: unitU,
     CTFig #: (sizedF fig scale)
     ]
  return figureE
-- book

-- book: modification
-- modify some properties of entities
x e l = updateE e  CTPos (\(Vec3 x y z) -> (Vec3 (x + l) y z))
rx e a = updateE e CTOri (\ori -> ori `multU` (rotU vec3X a))
ry e a = updateE e CTOri (\ori -> ori `multU` (rotU vec3Y a))

-- add some loop functions
start f sl = forkIO (let f' = f >> sleepFor sl >> f' in f')
stop tid = killThread tid
-- book

example = do

  {-
    You can try the single commands below also separately in the ghci interpreter.
    Fire up emacs, load this small script, then type C-c C-l to load it in 
    inferior mode (ghci shell in Emacs). Then try the single commands in
    the emacs ghci window!
  -}

  -- start the system, by forking the world in another thread and adding entities to it
  world <- forkGraphics3DWorld (msecT 30)
  (sceneE, lightE1, lightE2) <- create3DEnvironment
  mapM (addToWorld world) [sceneE, lightE1, lightE2]

  -- create some geometries and add them to the world
  g1 <- newF dodRed 1.0
  addToWorld world g1

  g2 <- newF cubeGreen 0.02
  addToWorld world g2
  x g2 4.0

  g3 <- newF ikoBlue 1.8
  addToWorld world g3
  x g3 (-4.0)

  -- start some movement
  t1 <- start (rx g1 0.02) (msecT 30)
  t2 <- start (ry g2 0.02) (msecT 10)
  t3 <- start (rx g3 0.01 >> ry g3 0.02) (msecT 20)

  return (sceneE, g1, g2, g3, t1, t2, t3)

  {- 

  If you do not want to go step by step, start example with:

  (sceneE, g1, g2, g3, t1, t2, t3) <- example

  then you can stop the simple movements with 
  stop t1
  stop t2
  stop t3

  -}


-- we also can compile into executable
main = do
  (sceneE, g1, g2, g3, t1, t2, t3) <- example
  qvar <- regQuitHandler sceneE
  takeMVar qvar

