{-# LANGUAGE OverloadedStrings #-}

module Main  where

import HGamer3D

import qualified Data.Text as T
import Control.Concurrent
import Control.Monad

-- define materials

mats = [
    matOrangeCrossMetal,
    matCrossMetal,
    matCrossMetalBlue,
    matMetal,
    matMetalZigZag,
    matMetalBumps,
    matFishEye,
    matMetalOrnament,
    matMetalScratch,
    matMetalLine,
    matGreenGrass,
    matBrownGrass,
    matGreyGrass,
    matSand,
    matRedRock,
    matBlackRock,
    matBrownStone,
    matStoneMetalWall,
    matCoalWall,
    matBrickWallGray,
    matBrickWallRed,
    matTilesOrange,
    matWoodTiles,
    matColourTiles,
    matBlackTiles
  ] 


-- START SYSTEM
---------------

go = do

  hg3d <- configureHG3D

  cam <- newE [
       ctCamera #: FullViewCamera,
       ctPosition #: Vec3 1.0 1.0 (-30.0),
       ctOrientation #: unitU
       ]

  li <- newE [
     ctLight #: Light PointLight 1.0 1000.0 1.0,
     ctPosition #: Vec3 1.0 1.0 (-30.0)
     ]

  return (hg3d, li, cam)

-- CONTENT CREATION

-- create cube with material, position at n of m
makeCube mat n m = do
  eCube <- newE [
     ctGeometry #: ShapeGeometry Cube,
     ctMaterial #: mat,
     ctScale #: Vec3 10.0 10.0 10.0,
     ctPosition #: rotate3 (2.0 * pi * (fromIntegral n) / (fromIntegral m)) vec3Y (Vec3 0.0 0.0 70.0),
--     ctPosition #: rotate3 (2.0 * pi * (fromIntegral n) / (fromIntegral m)) vec3Y (Vec3 2000.0 0.0 0.0),
     ctOrientation #: unitU
     ]
  return eCube

allCubes mats = do
         let m = length mats
         cubes <- mapM (\(mat, n) -> makeCube mat n m) (zip mats [1..m])
         return cubes

camy c d = do
     updateC c  ctOrientation (\u -> (rotU vec3Y d) .*. u)
     updateC c ctPosition (\p -> rotate3 d vec3Y p)

rotateWorld c cs d = forever $ do 
                                            camy c 0.005
                                            mapM (\c -> updateC c ctOrientation (\u -> (rotU vec3Y d) .*. u)) cs
                                            sleepFor (msecT 20)


demo = do
     (hg3d, l1, c) <- go
     cubes <- allCubes mats
     forkIO $ loopHG3D hg3d (msecT 20) (return True) -- allow close on windows click
     rotateWorld c cubes (-0.05)
     return ()

main = do
      demo 
      return ()

