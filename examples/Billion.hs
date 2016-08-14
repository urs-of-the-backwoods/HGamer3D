{-
    Sample: Billion, the number 1000.000.000
	HGamer3D Library (A project to enable 3D game development in Haskell)
	Copyright 2011-2015 Peter Althainz
	
	Distributed under the Apache License, Version 2.0
	(See attached file LICENSE or copy at 
	http://www.apache.org/licenses/LICENSE-2.0)
 
	file: Samples/Billion.hs
-}

{-# LANGUAGE OverloadedStrings #-}


module Main where

import HGamer3D

import qualified Data.Text as T
import Control.Concurrent
import Control.Monad

import Data.List

-- CH3-2e

-- CH3-3s
-- small tool, to create entities
creator w l = newE w l

-- entity creation tools
camera w pos = creator w [
                ctCamera #: FullViewCamera,
                ctPosition #: pos
                ]
    
item w pos shape mat = creator w [
                ctMaterial #: mat,
                ctGeometry #: ShapeGeometry shape,
                ctPosition #: pos,
                ctScale #: unitVec3,
                ctOrientation #: unitU
                ]
    
light w pos = creator w [
                ctLight #: Light PointLight 1.0 1000.0 1.0,
                ctPosition #: pos,
                ctColour #: white
                ]
                
textOne w = creator w [  
                ctText #: "Billion\n"
                , ctScreenRect #: Rectangle 10 10 120 25
            ]

textTwo w = creator w [  
                ctText #: ""
                , ctScreenRect #: Rectangle 10 50 200 500
            ]

showText e t = setC e ctText t

type Cube = [[[Entity]]]

itemCube w shape offset mat = do
    let r = [2.5, 5.0 .. 25.0]
    cubes <-
        mapM (\z -> 
            mapM (\y -> 
                mapM (\x -> item w ((Vec3 x y z) &+ offset) shape mat) r
                ) r
            ) r
    return cubes
    
itemFromCube :: Cube -> Int -> Int -> Int -> Entity
itemFromCube cube x y z = (((cube !! z) !! y) !! x)

lineFromCube :: Cube -> Int -> Int -> [Entity]
lineFromCube cube y z = ((cube !! y) !! z)

planeFromCube :: Cube -> Int -> [[Entity]]
planeFromCube cube z = (cube !! z)

coordFromN n = (x, y, z) where 
                    z = n `div` 100
                    y = (n - z*100) `div` 10
                    x = (n - z*100 - y*10) 

data CameraSpeed = Slow
                | Fast
                deriving (Eq, Show)

data CameraDirection = PosX | NegX | PosY | NegY | PosZ | NegZ deriving (Eq, Show)

data Command = MoveCamera [(CameraSpeed, CameraDirection)] GameTime
               | SetText T.Text
               | Cubes Shape Vec3 Material
               | SelectCube Int
               | MaterialLine Int Int Material
               | MaterialPlane Int Material
               | Material Int Int Int Material
               | Wait GameTime
                deriving (Eq, Show)

mUX = Vec3 0.02 0.0 0.0
mUY = Vec3 0.0 0.02 0.0
mUZ = Vec3 0.0 0.0 0.02
fUX = Vec3 0.05 0.0 0.0
fUY = Vec3 0.0 0.05 0.0
fUZ = Vec3 0.0 0.0 0.05

commandInterpreter w cam t2 refCubes refSel cmd = do
    case cmd of
        MoveCamera listOfMoves deltaTime  -> do
            let oneMove (s, d) = do
                case (s, d) of
                    (Slow, PosX) -> updateC cam ctPosition (\v -> v &+ mUX)
                    (Slow, NegX) -> updateC cam ctPosition (\v -> v &- mUX)
                    (Slow, PosY) -> updateC cam ctPosition (\v -> v &- mUY)
                    (Slow, NegY) -> updateC cam ctPosition (\v -> v &+ mUY)
                    (Slow, PosZ) -> updateC cam ctPosition (\v -> v &+ mUZ)
                    (Slow, NegZ) -> updateC cam ctPosition (\v -> v &- mUZ)
                    (Fast, PosX) -> updateC cam ctPosition (\v -> v &+ fUX)
                    (Fast, NegX) -> updateC cam ctPosition (\v -> v &- fUX)
                    (Fast, PosY) -> updateC cam ctPosition (\v -> v &- fUY)
                    (Fast, NegY) -> updateC cam ctPosition (\v -> v &+ fUY)
                    (Fast, PosZ) -> updateC cam ctPosition (\v -> v &+ fUZ)
                    (Fast, NegZ) -> updateC cam ctPosition (\v -> v &- fUZ)
            let moves startT = do
                t <- getTime
                if t < (startT + deltaTime) then do
                    mapM oneMove listOfMoves
                    sleepFor (msecT 50)
                    moves startT
                    else
                        return ()
            tNow <- getTime
            forkIO (moves tNow)
            return ()
    
        SetText t -> showText t2 t
        
        Cubes shape off mat -> do
            cubes <- readVar refCubes
            newCube <- itemCube w shape off mat
            writeVar refCubes (cubes ++ [newCube])
            writeVar refSel newCube
            return ()
            
        SelectCube i -> do
            cubes <- readVar refCubes
            writeVar refSel (cubes !! i)
            return ()
            
        MaterialLine y z mat -> do
            cube <- readVar refSel
            mapM (\c -> setC c ctMaterial mat) (lineFromCube cube y z)
            return ()
            
        MaterialPlane z mat -> do
            cube <- readVar refSel
            mapM (\c -> setC c ctMaterial mat) (concat (planeFromCube cube z))
            return ()
            
        Material x y z mat -> do
            cube <- readVar refSel
            setC (itemFromCube cube x y z) ctMaterial mat
            return ()
            
        Wait time -> do 
            sleepFor time
            
-- CAMERA MOVEMENT

installKeyPressed :: HG3D -> Entity -> Var [T.Text] -> IO ()
installKeyPressed w eventHandler varKeysPressed = do
    let handleKeys ke = do
        case ke of  
            KeyUp _ _ k -> updateVar varKeysPressed (\keys -> (filter (\k' -> k' /= k) keys, ()))
            KeyDown _ _ k -> updateVar varKeysPressed (\keys -> (if not (k `elem` keys) then k:keys else keys, ()))
            _ -> return ()
    registerCallback w eventHandler ctKeyEvent (\key -> handleKeys key)

installMoveCamera cam varKeysPressed = do
    let move = do
        keys <- readVar varKeysPressed
        if "A" `elem` keys then updateC cam ctPosition (\v -> v &- mUX) else return ()
        if "D" `elem` keys then updateC cam ctPosition (\v -> v &+ mUX) else return ()
        if "W" `elem` keys then updateC cam ctPosition (\v -> v &+ mUY) else return ()
        if "S" `elem` keys then updateC cam ctPosition (\v -> v &- mUY) else return ()
        if "E" `elem` keys then updateC cam ctPosition (\v -> v &+ mUZ) else return ()
        if "C" `elem` keys then updateC cam ctPosition (\v -> v &- mUZ) else return ()
        return ()
    forkIO $ forever $ move >> sleepFor (msecT 50)
    return()
    
installChangeCubes w ieh cubes = do
    let handleKeys k = do
        case k of
            KeyUp _ _ "Right" -> mapM (\c -> setC c ctMaterial matYellow) (lineFromCube cubes 0 0) >> return ()
            KeyUp _ _ "Up" -> mapM (\c -> setC c ctMaterial matRed) (concat (planeFromCube cubes 0)) >> return ()
            KeyUp _ _ "G" -> setC (((cubes !! 3) !! 4) !! 5) ctMaterial matGreen >> return ()
            KeyUp _ _ k -> return ()
            _ -> return ()
            
    registerCallback w ieh ctKeyEvent (\key -> handleKeys key)

installText w ieh t2 = do
    let handleKeys k = do
        case k of
            KeyUp _ _ "F1" -> showText t2 "imagine each cube is one year ...\n"
            KeyUp _ _ "F2" -> showText t2 "imagine each cube is one year\nthere are 10 yellow cubes, 10 years ...\n"
            KeyUp _ _ "F3" -> showText t2 "imagine each cube is one year\nthere are 10 yellow cubes, 10 years\nthere are 100 yellow and red cubes, 100 years ...\n"
            KeyUp _ _ "F4" -> showText t2 "imagine each cube is one year\nthere are 10 yellow cubes, 10 years\nthere are 100 yellow and red cubes, 100 years\nthere are 1000 cubes in total, 1000 years ..."
            _ -> return ()
    registerCallback w ieh ctKeyEvent (\key -> handleKeys key)
    
gameLogic w = do

    c <- camera w (Vec3 0.0 0.0 0.0)
    
    cubes <- itemCube w Pyramid zeroVec3 matBlue

    l <- light w (Vec3 10.0 10.0 0.0)
    ieh <- creator w [ctInputEventHandler #: DefaultEventHandler, ctKeyEvent #: NoKeyEvent] 
    t1 <- textOne w
    t2 <- textTwo w
    refK <- makeVar []
    
    installKeyPressed w ieh refK
    installMoveCamera c refK
    installChangeCubes w ieh cubes
    installText w ieh t2

    refCubes <- makeVar [cubes]
    refSel <- makeVar cubes

    mapM (\cmd -> commandInterpreter w c t2 refCubes refSel cmd) (
        [
    
        -- Intro
        MoveCamera [(Slow, NegZ)] (secT 30),
        Wait (secT 10),
        SetText "imagine each pyramid is one year ...\n",
        Wait (secT 10),
        MaterialLine 0 0 matYellow,
        SetText "imagine each pyramid is one year\nthere are 10 yellow pyramids, 10 years ...\n",
        Wait (secT 10),
        MoveCamera [(Slow, NegZ), (Slow, NegY), (Slow, PosX)] (secT 35),
        Wait (secT 35),
        MoveCamera [(Slow, NegZ)] (secT 10),
        SetText "imagine each cube is one year\nthere are 100 red pyramids, 100 years ...\n",
        MaterialPlane 0 matRed,
        Wait (secT 10),
        SetText "imagine each cube is one year\nthere are 1000 blue pyramids, 1000 years ...",
        MaterialPlane 0 matBlue,
        Wait (secT 10),
        
        -- How to count years
        SetText "imagine each cube is one year\nthere are 1000 blue pyramids, 1000 years ...\n\ncounting starts at lower left front corner"
        
        ] ++ 
        
        concatMap (\n -> let
                            (x, y, z) = coordFromN (n-1)
                         in
                            [
                                Wait (msecT 300),
                                SetText (T.pack ("imagine each cube is one year\nthere are 1000 blue pyramids, 1000 years ...\n\ncounting starts at lower left front corner\nYear " ++ (show n))),
                                Material x y z matGreen
                            ] ) [1..200]
                            
        ++
        
        [        
            MoveCamera [(Slow, NegZ)] (secT 40)
        ] ++
        
        concatMap (\n -> let
                            (x, y, z) = coordFromN (n-1)
                         in
                            [
                                Wait (msecT 50),
                                SetText (T.pack ("imagine each cube is one year\nthere are 1000 blue pyramids, 1000 years ...\n\ncounting starts at lower left front corner\nYear " ++ (show n))),
                                Material x y z matGreen
                            ] ) [201..1000]
                            
        ++

        [
        
        SetText ("to be continued ..."),
        Wait (secT 30)
       
        ]
        -- 
        
        )

    exitHG3D w
    return ()
-- CH3-4e

    
main = do 
      runGame standardGraphics3DConfig gameLogic (msecT 20)
      return ()
