{-# LANGUAGE OverloadedStrings #-}

{-
    Sample: Cuboid2, a 3D puzzle game, thanks to Pedro Martins for game idea (https://github.com/pedromartins/cuboid)
    HGamer3D Library (A project to enable 3D game development in Haskell)
    Copyright 2011-2015 Peter Althainz
    
    Distributed under the Apache License, Version 2.0
    (See attached file LICENSE or copy at 
    http://www.apache.org/licenses/LICENSE-2.0)
 
    file: Samples/Cuboid.hs
-}

module Main where

import HGamer3D

import qualified Data.Text as T
import Control.Concurrent
import Control.Monad
import Data.Fixed
import Data.IORef
import Data.List


-- Game Logic, pure functional parts
------------------------------------

-- CH3-7s
-- largest dimension
maxDim :: Int
maxDim = 5

-- logical positions, range from 0 to dim - 1
type FieldPos = (Int, Int, Int) 

-- one level of positions, start and end position
data Level = Level {  lDim :: Int,
                      lField :: [FieldPos],
                      lStart :: FieldPos,
                      lGoal :: FieldPos      }  deriving (Show, Eq)

-- all game data, array of levels
type GameData = [Level]

gameData :: GameData   -- static data of all levels
gameData = [
  Level 5 [(2,2,2),(1,1,1),(3,3,3)] (0, 3, 4) (4, 3, 4) ,
  Level 5 [(0,4,4),(0,0,3),(4,1,3)] (0,4,3) (3,1,0),
  Level 5 [(1,3,0),(3,3,0),(1,1,1),(3,1,1),(3,4,2),(0,3,3),(1,2,4),(2,1,4),(3,0,4),(3,3,4),(4,2,4)] (0,3,4) (4,2,3)  
  ]

           
-- function, evaluating a move, moving the cursor in a specific direction, Bool is if success
steps :: FieldPos -> FieldPos -> Level -> ([FieldPos], Bool)
steps start move level =

    let atGoal p = p == (lGoal level)
        blocked p = p `elem` (lField level)
        outOfBounds (a, b, c) = let d = lDim level in a >= d || b >= d || c >= d || a < 0 || b < 0 || c < 0
        next (x, y, z) (x', y', z') = (x+x', y+y', z+z')
        
        oneStep ps start pos
            | atGoal pos = (ps ++ [pos], True)                        -- stopping at goal
            | blocked pos = (ps, False)                               -- stopping at current pos, blocked
            | outOfBounds pos = (ps ++ [pos, start], False)           -- stopping, due to outofbounds, back to start
            | otherwise = oneStep (ps ++ [pos]) start (next pos move) -- successful step do next step
            
    in oneStep [] start (next start move)
-- CH3-7e
          
    
-- Engine related low level routines
------------------------------------

-- setup basic 3D world
setupWorld hg3d = do
    -- camera and light
    cam <- newE hg3d [ctCamera #: FullViewCamera, ctPosition #: Vec3 0 0 0, ctOrientation #: unitU]
    -- CH3-8s
    light1 <- newE hg3d [ctLight #: Light (SpotLight (Deg 50) 1.0) 1.0 100.0 1.0, ctPosition #: Vec3 (10) (-10) (-10.0)]
    light2 <- newE hg3d [ctLight #: Light PointLight 0.5 1000.0 0.8, ctPosition #: Vec3 0 0 (-50)]
    light3 <- newE hg3d [ctLight #: Light DirectionalLight 1.0 1000.0 1.0, ctOrientation #: (rotU vec3Y 45 .*. rotU vec3X 45)]
    -- CH3-8e
    -- HGamer3D website, entities and events, event listener for keys
    -- key input
    ieh <- newE hg3d [ctInputEventHandler #: DefaultEventHandler, ctKeyEvent #: NoKeyEvent] 
    -- end of website text
    return (cam, ieh)

-- quat from 2 vectors, normalized input
ufrom2v u v = let (Vec3 x y z) = u &^ v in mkU (Vec4 (1.0 + (u &. v)) x y z )

-- create a line
line :: HG3D -> Material -> Vec3 -> Vec3 -> Float -> IO Entity
line hg3d m p1 p2 t = do
    let d = p2 &- p1
    let l = len d
    let u = ufrom2v (normalize (Vec3 0 l 0)) (normalize d) 
    l <- newE hg3d [ctMaterial #: m, ctGeometry #: ShapeGeometry Cube, 
                    ctPosition #: (p1 &+ (d &* 0.5)) , ctScale #: (Vec3 t l t), ctOrientation #: u]
    return l

-- create a sphere
sphere hg3d m p s = do
    s' <- newE hg3d [ctMaterial #: m, ctGeometry #: ShapeGeometry Sphere, 
                    ctPosition #: p, ctScale #: s, ctOrientation #: unitU]
    return s'

-- create a cube
cube hg3d m p s = do
    s' <- newE hg3d [ctMaterial #: m, ctGeometry #: ShapeGeometry Cube, 
                    ctPosition #: p, ctScale #: s, ctOrientation #: unitU]
    return s'

-- Containing cube
cubeOutline p s = let
    corners = [ Vec3 x y z | x <- [-1,1], y <- [-1,1], z <- [-1,1]]
    edges = filter (\(a, b) -> (a &- b) `elem` [Vec3 2 0 0, Vec3 0 (-2) 0, Vec3 0 0 2] ) [(ca, cb) | ca <- corners, cb <- corners]
    trans v = (v &! s) &+ p
    in (map trans corners, map (\(a, b) -> (trans a, trans b)) edges)

-- constants between fieldPos and real world
oC :: Vec3
oC = (Vec3 0 0 20.0)

sC :: Float
sC = 10.0

spC :: FieldPos
spC = (0, -10, 0)
 
-- cuboid position into vec3 positions
f2pos :: FieldPos -> Vec3
f2pos pos@(x, y, z) = let
    vs = Vec3 (fromIntegral x) (fromIntegral y) (fromIntegral z)
    dim = fromIntegral maxDim
    vs' = (vs &* (2.0 / (dim - 1))) &- unitVec3
    in oC &+ (sC *& vs')

-- draw the static frame around the cubes    
drawCubeFrame hg3d = do
    let (corners, edges) = cubeOutline oC (unitVec3 &* (sC * 1.05))
    mapM (\(a, b) -> line hg3d matMetal a b (0.05 * sC) ) edges
    mapM (\v -> sphere hg3d matMetal v (unitVec3 &* (sC * 0.12))) corners

-- create all used sphere
createSpheresAndCubes hg3d = do
    let sphereSize = unitVec3 &* (2.0 * sC / (fromIntegral maxDim))
    cubes <- mapM (\_ -> cube hg3d matBlue (f2pos spC) (sphereSize &* 0.9)) [0.. (maxDim * maxDim) -1]
    startSphere <- sphere hg3d matLime (f2pos spC) sphereSize
    endSphere <- sphere hg3d matRed (f2pos spC) sphereSize
    return (startSphere, endSphere, cubes)

setPos er fp = setC er ctPosition (f2pos fp)
  
-- Low Level Event Routines
---------------------------

-- HGamer3D website, entities and events, WASD logic
-- install key handler, moves each key up and currently pressed keys in variable
installKeyHandler :: HG3D -> Var [T.Text] -> Var [T.Text] -> Entity -> IO ()
installKeyHandler hg3d varKeysUp varKeysPressed ieh = do
    let handleKeys ke = do
                            case ke of  
                                KeyUp _ _ k -> do
                                    updateVar varKeysPressed (\keys -> (filter (\k' -> k' /= k) keys, ()))
                                    updateVar varKeysUp (\keys -> (keys ++ [k], ()))
                                    return ()
                                KeyDown _ _ k -> do
                                    updateVar varKeysPressed (\keys -> (if not (k `elem` keys) then k:keys else keys, ())) >> return ()
                                _ -> return () 
    registerCallback hg3d ieh ctKeyEvent (\k -> handleKeys k)
    return ()

-- camera movement
installMoveCamera cam varKeysPressed = do
    let mUX = (Vec3 0.3 0 0.0)
    let mUZ = (Vec3 0 0 0.3)
    let move = do
                    keys <- readVar varKeysPressed
                    if "D" `elem` keys then updateC cam ctPosition (\v -> v &+ mUX) else return ()
                    if "A" `elem` keys then updateC cam ctPosition (\v -> v &- mUX) else return ()
                    if "W" `elem` keys then updateC cam ctPosition (\v -> v &+ mUZ) else return ()
                    if "S" `elem` keys then updateC cam ctPosition (\v -> v &- mUZ) else return ()
                    return ()
    forkIO $ forever $ move >> sleepFor (msecT 50)
    return()
-- end of website text
    
-- Game Preparation Work
------------------------

startWorld hg3d = do
    (cam, inputHandler) <- setupWorld hg3d
    varKeysUp <- makeVar []
    varKeysPressed <- makeVar []
    installKeyHandler hg3d varKeysUp varKeysPressed inputHandler
    installMoveCamera cam varKeysPressed
    drawCubeFrame hg3d
    (startSphere, endSphere, cubes) <- createSpheresAndCubes hg3d
    return (varKeysUp, (startSphere, endSphere, cubes))


-- Game Actions
---------------
-- actions are fired and do something in the game world

-- action set new field
newFieldA (sS, eS, ss) level = do
    mapM (\s -> setPos s spC) ss
    mapM (\(s, p) -> setPos s p) (zip ss (lField level))
    setPos sS (lStart level)
    setPos eS (lGoal level)
    return ()

-- action move cursor, also fractional values are possible
moveA sS start end val = do
    let sV = f2pos start
    let eV = f2pos end
    let v = sV &+ ((eV &- sV) &* val)
    setC sS ctPosition v
    return ()

-- timeLoop, do a sequence of actions in a specific time
timeLoop :: [IO()] -> GameTime -> IO()
timeLoop actionList tRun = do
    let tDiff = usecT ((usec tRun) `div` (length actionList))
    mapM (\a -> sleepFor tDiff >> a) actionList
    return ()
    
-- move object slowly to next location
moveTime sS start end tRun = do
    let nSteps = 20
        steps = map (\d -> moveA sS start end d) (map (\n -> (fromIntegral n) / (fromIntegral nSteps)) [1..nSteps])
    timeLoop steps (msecT 500)

-- blink cursor a little bit
blinkCursor c = do
    timeLoop (map (\m -> setC c ctMaterial m) [matOlive, matGreen, matOlive, matGreen, matOlive, matGreen, matOlive, matLime]) (secT 2)
    return ()
        
getMoveFromKey k = case k of
                "Left" -> Just (-1, 0, 0)
                "Right" -> Just (1, 0, 0)
                "Up" -> Just (0, 1, 0)
                "Down" -> Just (0, -1, 0)
                "PageUp" -> Just (0, 0, 1)
                "PageDown" -> Just (0, 0, -1)
                _ -> Nothing

-- CH3-11s                
runLevel varKeysUp allS@(sS, eS, ss) level = do

    newFieldA allS level
    let moveSteps pos list = case list of
            (p: ps) -> moveTime sS pos p (msecT 200) >> moveSteps p ps
            [] -> return ()
        resetKeys = writeVar varKeysUp []
        pause = sleepFor (msecT 20)
        getMove = do
            keys <- updateVar' varKeysUp (\ks -> ([], ks))
            if length keys > 0 
                then case getMoveFromKey (head keys) of
                        Just m -> return m
                        Nothing -> pause >> getMove
                else pause >> getMove
        processMove pos m = case steps pos m level of
            ([], False) -> blinkCursor sS >> return (pos, False)
            (steps, False) -> moveSteps pos steps >> return (((head . reverse) steps), False)
            (steps, True) -> moveSteps pos steps >> return (((head . reverse) steps), True)
        loopKey pos = do
            m <- getMove
            (pos', success) <- processMove pos m
            if success then return () else loopKey pos'
    resetKeys
    loopKey (lStart level)
-- CH3-11e
            
gameLogic hg3d = do
    (varKeysUp, allS) <- startWorld hg3d
    mapM (\l -> runLevel varKeysUp allS l) gameData
    return ()
    
main = do
    runGame standardGraphics3DConfig gameLogic (msecT 20)
    return ()

{-

:s OverloadedStrings

-- start
:l Cuboid2
:import Control.Concurrent
forkIO $ main

-}
