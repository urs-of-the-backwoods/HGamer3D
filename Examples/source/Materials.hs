module Main  where

import HGamer3D.Data
import HGamer3D.Common
import HGamer3D.Graphics3D

import Control.Concurrent
import Control.Monad

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
  "Wall/Brick1",
  "Wall/Brick2",
  "Wall/Stone1"
  ]


-- START SYSTEM
---------------

go = do

  -- initialize hg3d
  world <- forkGraphics3DWorld (msecT 30)

  -- define a camera, lights, events and env entity
  let camera1 = Camera (Frustum 5.0 5000.0 (Deg 40)) (Viewport 0 (Rectangle 0.0 0.0 1.0 1.0) black)

  cam <- newE [
       CTCam #: camera1,
       CTPos #: Vec3 0.0 0.0 (-1500.0),
       CTOri #: unitU
       ]

  env <- newE [
       CTScP #: SceneParameter white NoShadows (SkyBox (ResourceMaterial "SkyBox/BlueSpace") 10.0)
       ]

  let light1 = Light white white PointLight 
  li <- newE [
     CTLig #: light1,
     CTPos #: (zeroVec3 &+ (Vec3 0.0 0.0 0.0))
     ]

  mapM (addToWorld world) [env, cam, li]  

  return (world, env, li, cam)

-- CONTENT CREATION

-- create cube with material, position at n of m
makeCube systems mat n m = do
  -- calc pos
  let pos = rotate3 (2.0 * pi * (fromIntegral n) / (fromIntegral m)) vec3Y (Vec3 2000.0 0.0 0.0)
  -- create cube
  let cube = SimpleFigure Cube (ResourceMaterial mat)
  eCube <- newE [
     CTPos #: pos,
     CTOri #: unitU,
     CTFig #: cube
     ]
  addToWorld systems eCube
  return eCube

allCubes s mats = do
         let m = length mats
         cubes <- mapM (\(mat, n) -> makeCube s mat n m) (zip mats [1..m])
         return cubes

camy c d = do
     updateE c  CTOri (\u -> (rotU vec3Y d) .*. u)
     updateE c CTPos (\p -> rotate3 d vec3Y p)

rotateWorld c cs d = forkIO $ forever $ do 
                                            camy c 0.001
                                            mapM (\c -> updateE c CTOri (\u -> (rotU vec3Y d) .*. u)) cs
                                            sleepFor (msecT 30)


demo = do
     (s, e, l1, c) <- go
     cubes <- allCubes s mats
     rotateWorld c cubes 0.002
     return e

main = do
     e <- demo
     qvar <- regQuitHandler e
     takeMVar qvar
