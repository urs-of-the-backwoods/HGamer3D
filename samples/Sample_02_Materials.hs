{-# LANGUAGE OverloadedStrings #-}

module Sample_02_Materials  where

import HGamer3D

import qualified Data.Text as T
import Control.Concurrent
import Control.Monad
import SampleRunner

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


makeCube hg3d mat n m = do
  eCube <- newE hg3d [
     ctGeometry #: ShapeGeometry Cube,
     ctMaterial #: mat,
     ctScale #: Vec3 10.0 10.0 10.0,
     ctPosition #: rotate3 (2.0 * pi * (fromIntegral n) / (fromIntegral m)) vec3Y (Vec3 0.0 0.0 70.0),
     ctOrientation #: unitU
     ]
  return eCube

allCubes hg3d mats = do
         let m = length mats
         cubes <- mapM (\(mat, n) -> makeCube hg3d mat n m) (zip mats [1..m])
         return cubes

camy c d = do
     updateC c ctOrientation (\u -> (rotU vec3Y d) .*. u)
     updateC c ctPosition (\p -> rotate3 d vec3Y p)

rotateWorld c cs d quitV =  do
                                            camy c 0.005
                                            mapM (\c -> updateC c ctOrientation (\u -> (rotU vec3Y d) .*. u)) cs
                                            sleepFor (msecT 20)
                                            q <- readVar quitV
                                            if not q
                                              then rotateWorld c cs d quitV
                                              else return ()

creator hg3d c = do
     quitV <- makeVar False
     cubes <- allCubes hg3d mats
     forkIO $ rotateWorld c cubes (-0.05) quitV
     return (cubes, quitV, c)

destructor (cubes, quitV, c) = do
  writeVar quitV True
  sleepFor (msecT 500) -- monitor that motion stops before deletion
  mapM delE cubes
  setC c ctOrientation unitU
  setC c ctPosition (Vec3 1.0 1.0 (-30.0))
  return ()

sampleRunner hg3d c = SampleRunner (return ()) (do
                                    state <- creator hg3d c
                                    return (SampleRunner (destructor state) (return emptySampleRunner) ))


