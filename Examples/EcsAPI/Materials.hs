module Main  where

import HGamer3D.Data
import HGamer3D.Engine.EcsAPI
import HGamer3D.Graphics3D.EcsAPI
import Control.Concurrent

-- define materials

mats = [
  "Floor/Tiles1",
  "Floor/Tiles2",
  "Floor/Tiles3",
  "Floor/Wood1",
  "Floor/Steel1",
  "Floor/Steel2",
  "Floor/Steel3",
  "Floor/Steel3",
  "Floor/Steel4",
  "Floor/Plain2",
  "Floor/Plain2S",
  "Floor/Plain3",
  "Floor/Plain3S",
  "Floor/Steel5",
  "Floor/Steel6",
  "Floor/Steel7",
  "Floor/Stone1",
  "Floor/Stone2",
  "Floor/Stone3",
  "Ground/Grass1",
  "Ground/Grass2",
  "Ground/Grass3",
  "Ground/Sand1",
  "Ground/Sand2",
  "SkyBox/BlueSky1",
  "Wall/Brick1",
  "Wall/Brick2",
  "Wall/Stone1"
  ]


-- START SYSTEM
---------------

go = do

  -- initialize hg3d
  ecsG3D <- runSystemGraphics3D (msecT 30)
  ecsEvt <- runSystemEvent (msecT 110)
  let systems = ecsG3D #+ ecsEvt #+ []

  -- define a camera, lights, events and env entity
  let camera1 = Camera (Frustum 5.0 5000.0 (Deg 40)) (Viewport 0 (Rectangle 0.0 0.0 1.0 1.0) black)
  let events1 = EventReceiver [ApplicationEvents]

  cam <- entity [
       CTCam #: camera1,
       CTPos #: Vec3 0.0 0.0 (-1500.0),
       CTOri #: unitU
       ]

  env <- entity [
       CTScP #: SceneParameter white NoShadows (SkyBox (ResourceMaterial "SkyBox/BlueSpace") 10.0),
       CTEvR #: events1
       ]

  let light1 = Light white white PointLight 
  li <- entity [
     CTLig #: light1,
     CTPos #: (zeroVec3 &+ (Vec3 0.0 0.0 0.0))
     ]

  mapM (addToWorld systems) [env, cam, li]  

  return (systems, env, li, cam)

-- CONTENT CREATION

-- create cube with material, position at n of m
makeCube systems mat n m = do
  -- calc pos
  let pos = rotate3 (2.0 * pi * (fromIntegral n) / (fromIntegral m)) vec3Y (Vec3 2000.0 0.0 0.0)
  -- create cube
  let cube = SimpleFigure Cube (ResourceMaterial mat)
  eCube <- entity [
     CTPos #: pos,
     CTOri #: unitU,
     CTFig #: cube
     ]
  addToWorld systems eCube
  rotCube eCube 0.01
  return eCube

allCubes s mats = do
         let m = length mats
         mapM (\(mat, n) -> makeCube s mat n m) (zip mats [1..m])

camy c d = do
     updateC (c # CTOri) (\u -> (rotU vec3Y d) .*. u)
     updateC (c # CTPos) (\p -> rotate3 d vec3Y p)

rotCam c = forkIO $ let 
                      r = do
                                camy c 0.001
                                threadDelay (usec (msecT 30))
                                r
                      in r

rotCube c d = forkIO $ let 
                      r = do
                                updateC (c # CTOri) (\u -> (rotU vec3Y d) .*. u)
                                threadDelay (usec (msecT 30))
                                r
                      in r

demo = do
     (s, e, l1, c) <- go
     allCubes s mats
     t <- rotCam c
     return (s, c)

main = do
     demo
     let w = do
                        threadDelay (usec (secT 3600))
                        w
     w