{-# LANGUAGE Arrows, DeriveDataTypeable, ExistentialQuantification #-}

-- Cuboid2, a 3D puzzle game, thanks to Pedro Martins for game idea (https://github.com/pedromartins/cuboid)
--
-- (c) 2013 Peter Althainz
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- You may obtain a copy of the License at
--
--     http://www.apache.org/licenses/LICENSE-2.0
--
-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS,
-- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
-- See the License for the specific language governing permissions and
-- limitations under the License.

module Main where

import Control.Wire as W
import Control.Wire.Unsafe.Event as U
import Prelude hiding ((.), id)
import Data.Maybe
import Data.Dynamic

import HGamer3D
import HGamer3D.Audio
import HGamer3D.InputSystem

import GameWire
import EntityComponentSystem


-- GEOMETRY DATA AND 3D SETUP
type FieldVec = (Int, Int, Int) 

data Cuboid = Cuboid {  cbDim :: Int,
                        cbField :: [FieldVec],
                        cbStart :: FieldVec,
                        cbGoal :: FieldVec      }  deriving (Show, Eq, Typeable)

type Level = [Cuboid]

oneAndOnlyLevel :: Level   -- the only level, we currently have
oneAndOnlyLevel = [
  Cuboid 5 [(2,2,2),(1,1,1),(3,3,3)] (0, 3, 4) (4, 3, 4) ,
  Cuboid 5 [(0,4,4),(0,0,3),(4,1,3)] (0,4,3) (3,1,0),
  Cuboid 5 [(1,3,0),(3,3,0),(1,1,1),(3,1,1),(3,4,2),(0,3,3),(1,2,4),(2,1,4),(3,0,4),(3,3,4),(4,2,4)] (0,3,4) (4,2,3)  ]

-- moving the cursor within the cube
data CursorResult = CursorStepped | 
                    CursorOutOfBounds | 
                    CursorSuccess            deriving (Show, Eq, Typeable)

stepCursor :: FieldVec -> FieldVec -> Cuboid -> (FieldVec, CursorResult)
stepCursor start (x, y, z) cubo = let 
  dim = cbDim cubo
  (x', y', z') = start
  pos = (x'+x, y'+y, z'+z)
  (a, b, c) = pos
  (a', b', c') = cbGoal cubo
  (fv, rval) 
    | (a == a' && b == b' && c == c') = (pos, CursorSuccess)
    | elem pos (cbField cubo) = (start, CursorStepped)
    | a < 0 || a >= dim || b < 0 || b >= dim || c < 0 || c >= dim = (pos, CursorOutOfBounds)
    | otherwise = stepCursor pos (x, y, z) cubo
  in (fv, rval)


-- cuboid position into vec3 positions, depending on cuboid dimension
cuboFieldToPos :: Cuboid -> FieldVec -> Vec3
cuboFieldToPos cubo (x, y, z) = (Vec3 (posFromI x) (posFromI y) (posFromI z)) where
  dim = fromIntegral (cbDim cubo)
  step = scaleC / dim
  start = step * (1- dim) / 2.0
  posFromI i = start + ((fromIntegral i) * step)
  
-- scaling1 constants, geometry constants
scaleC = 30.0
sizeC = scaleC * 0.9 / 100.0 -- standard creation routines of cube and sphere create 100.0 large elements (diameter)
timeMoveC = 1.0
nCubes = 12
cameraPosC = Vec3 5.0 5.0 80.0
cameraLookAtC =  Vec3 0.0 (5.0) 0.0
vcreate = Vec3 100.0 0.0 0.0

materialBlue = ResourceMaterial "Colours/Blue"
materialRed = ResourceMaterial "Colours/Red"
materialGreen = ResourceMaterial "Colours/Green"

createMeshes = let 
  redMesh = sphereMesh
  greenMesh = sphereMesh
  blueMesh = cubeMesh
  in (redMesh, greenMesh, blueMesh)

createElement g3ds mesh material pos size = do
          elem3d <- object3DFromMesh g3ds mesh (Just material) False
          positionTo3D elem3d pos
          scaleTo3D elem3d (Vec3 size size size)
          return elem3d
          
_createLine g3ds a b colour = do
  m <- colouredLineMesh g3ds a b colour
  elem3d <- object3DFromMesh g3ds m Nothing False
  return elem3d
          
create3dLines g3ds cubo = do
          let lineColour = Colour 1.0 1.0 1.0 1.0
          let dim = 5
          let linePairs = let
                m = (cbDim cubo)-1
                (a, b, c, d) = ( (0,0,0),(m, 0, 0),(m, m, 0),(0, m, 0) )
                (a', b', c', d') = ( (0, 0, m), (m, 0, m), (m, m, m), (0, m, m)  )
                in [ (a, b), (b, c), (c, d), (d, a), (a', b'), (b', c'), (c', d'), (d', a'), (a, a'), (b, b'), (c, c'), (d, d')]
          lines  <- mapM (\(a, b) -> do
                             line <- _createLine g3ds a b lineColour
                             return line
                             )  (fmap ( \(a, b) -> ((cuboFieldToPos cubo a), (cuboFieldToPos cubo b) )) linePairs)
          let fc = 1.0 + (1.0 / ( (fromIntegral (cbDim cubo)) - 1 ) )
          mapM (\l -> scaleTo3D l (Vec3 fc fc fc)) lines
          return lines
          
createSpheres g3ds redMesh greenMesh = do          
        let size = sizeC / 5.0 
        sphereRed <- createElement g3ds redMesh materialRed vcreate size
        sphereGreen <- createElement g3ds greenMesh materialGreen vcreate size
        return (sphereRed, sphereGreen)
  
createCubes g3ds blueMesh = do
        let size = sizeC / (fromIntegral 5) 
        cubes <- mapM (\v -> createElement g3ds blueMesh materialBlue vcreate size) [0..nCubes-1]
        return cubes
        
cubeStartPos :: Int -> Vec3  
cubeStartPos = (\x -> if even x then Vec3 (-60.0) 0.0 0.0  else Vec3 60.0 0.0 0.0)

createSounds :: IO [AudioSource]
createSounds = do
  setAudioMainVolume 100.0
  msounds <- mapM createSound ["pling.wav", "atone.wav", "dundundun.wav",  "magicwand.wav"]
  return $ map fromJust msounds 
  

-- GAME COMMANDS AND LOGIC

-- most game command wires are made up of a switch, which generates a new wire, once a command of specific type is received
-- most of the logic is sending and receiving events of specific types

-- generic switch, in event is two-fold, left side creates new wire of a -> b
-- right side is a output is b, runs sequentially, first do new wire switched in, then restart switch logic
gameSwitch :: (gst -> GameWire (W.Event a) (W.Event b)) -> GameWire (W.Event gst, W.Event a) (W.Event b)
gameSwitch inWireF = switch $ mkPure_ (\(gsEvt, aEvt) -> case gsEvt of
              U.Event gs -> let
                newWire = (inWireF gs) . mkPure_ (\(a, b) -> Right b) --> (gameSwitch inWireF)
                in Right (U.NoEvent, U.Event (newWire))
              _ -> Right (U.NoEvent, U.NoEvent) )

-- helper wire, runs a switch in case only one input provided
runWithOneEvtW :: GameWire (W.Event a) (W.Event a, W.Event b)
runWithOneEvtW = (mkPure_ (\evtC -> Right (evtC, U.NoEvent)))


-- runLevelW - receives two event, one left, one right
-- left event switches into new cycle of cuboids (this is one level)
-- right event simply gets the next cuboid from the current level
runLevelW :: GameWire (W.Event Level, W.Event a) (W.Event Cuboid)
runLevelW = gameSwitch (\level -> cycleW level)
                                             
-- this wire receives a new cube and then removes all the participants into the right places
-- after all is finalized, it sends the new cube to the instance handling the keypresses
-- 
buildCuboidW :: Entity -> Entity -> [Entity] -> GameWire (W.Event Cuboid, W.Event a) (W.Event Cuboid)
buildCuboidW redSphere greenSphere blueCubes = gameSwitch (\cubo -> wireMove cubo redSphere greenSphere blueCubes) where
  
  wireMove cubo redSphere greenSphere blueCubes = let 
    
    posOutArr =   map (\i -> ( blueCubes !! i, cubeStartPos i) ) [(length (cbField cubo)) .. (nCubes-1)]
    moveCubesOut = foldl1 (-->) (fmap (\(e, pos) -> moveTo e pos 0.1) posOutArr)
    moveCubesIn = foldl1 (-->) (fmap (\(e, pos) -> moveTo e pos 0.5)   
                                (zip blueCubes (fmap (cuboFieldToPos cubo) (cbField cubo))))
    moveSpheres = moveTo redSphere (cuboFieldToPos cubo (cbStart cubo)) 0.5 --> 
                  moveTo greenSphere (cuboFieldToPos cubo (cbGoal cubo)) 0.5 
    in pure (U.NoEvent) . moveCubesOut --> pure (U.NoEvent) . moveSpheres --> pure (U.NoEvent) . moveCubesIn 
       --> sendOnce . now . pure cubo
 
getStepsFromKeyW :: Entity -> GameWire () (W.Event FieldVec)
getStepsFromKeyW combobj = (proc _ -> do
                               q <- mkGen_ (\e -> do u <- getOrientation e; return $ Right (fromJust u)) -< combobj
                               let v = q `actU` (Vec3 0.0 0.0 1.0)
                               let (y, z) = (_2 v, _3 v)
                               let (upFv, downFv, qFv, aFv) = ((0, 1, 0), (0, -1, 0), (0, 0, -1), (0, 0, 1))
                               let (upFv', downFv', qFv', aFv')
                                     | z > 0.5  = (upFv, downFv, qFv, aFv)
                                     | y < -0.5 = (qFv, aFv, downFv, upFv) 
                                     | z <= -0.5 = (downFv, upFv, aFv, qFv)
                                     | y >= 0.5 = (aFv, qFv, upFv, downFv)
                                                  
                               dir <- keyW KeyLeft . pure "l" <& keyW KeyRight . pure "r" <&
                                      keyW KeyUp . pure "u" <& keyW KeyDown . pure "d" <& 
                                      keyW KeyPageUp . pure "q" <& keyW KeyPageDown . pure "a" -< ()
                                      
                               let dir'' = case dir of
                                               U.Event dir' -> U.Event $ fromJust $ lookup dir' 
                                                               [("l",(-1, 0, 0)), ("r",(1, 0, 0)), ("u",upFv'), 
                                                                ("d",downFv'), ("q",qFv'), ("a",aFv')] 
                                               U.NoEvent -> U.NoEvent
                           
                               returnA -< dir'')
                           

-- A state is modified by a function, until a predicate is met.
-- In between each step a wire with the old and new state is run.

stateStepperW :: a -> (a -> b -> a) -> (a -> Bool) -> (a -> a -> (GameWire (W.Event b) (W.Event ())) ) -> GameWire (W.Event b) (W.Event ())
stateStepperW startState stateF pred wire = switch $ mkPure_ (\evtB -> case evtB of
                          U.Event val -> let
                                         nextState = stateF startState val
                                         finish = pred nextState
                                         nextWire = if finish then sendOnce . now . pure () else stateStepperW nextState stateF pred wire
                                         in Right (U.NoEvent, U.Event (wire startState nextState --> nextWire))
                          U.NoEvent -> Right (U.NoEvent, U.NoEvent))



-- this wire runs one cuboid and upon completion, it sends an anonymous event
runCuboidW :: [AudioSource] -> Entity -> GameWire (W.Event Cuboid, W.Event FieldVec) (W.Event ())
runCuboidW sounds redSphere = gameSwitch (\cuboid -> processCuboW cuboid ) where
  
  processCuboW cubo = stateStepperW startState stateF pred wire where
    
    startState = (cbStart cubo, CursorStepped) 
    stateF = \(pos, res) nextStepFV -> if res == CursorOutOfBounds then 
                                         stepCursor (cbStart cubo) nextStepFV cubo
                                       else
                                         stepCursor pos nextStepFV cubo
    pred = \(pos, res) -> res == CursorSuccess
    wire (startPos, startRes) (endPos, endRes) = let
                      startSound = 0
                      stopSound = case endRes of
                        CursorOutOfBounds -> 2
                        CursorSuccess -> 3
                        CursorStepped -> 1
                      moveToPos = moveTo redSphere (cuboFieldToPos cubo endPos ) 1.0
                      moveToEnd = if endRes == CursorOutOfBounds then 
                                     moveTo redSphere (cuboFieldToPos cubo (cbStart cubo)) 1.0
                                  else mkEmpty
                      in playSoundW sounds startSound --> pure (U.NoEvent) . moveToPos --> 
                         playSoundW sounds stopSound --> pure (U.NoEvent) . moveToEnd --> mkEmpty


playSoundW sounds i = mkEmpty . soundW (sounds !! i) . now . pure ()
                      
-- THE MAIN PROGRAM
    
renderLoop g3ds guis s w cmd = do
  (ds, s') <- stepSession s
  (cmd', w') <- stepWire w ds (Right cmd)
  let cmd'' = case cmd' of
        (Right (U.Event cmd''')) -> (U.Event cmd''')
        _ -> U.Event CmdNoOp
  (evt, qFlag) <- loopHGamer3D g3ds guis
  if qFlag then return () else renderLoop g3ds guis s' w' cmd''
  
main = do
  (g3ds, guis, camera, viewport) <- initHGamer3D "HGamer3D - Cuboid2" True False True
  
  positionTo3D camera cameraPosC
  cameraLookAt camera cameraLookAtC
  let white = Colour 1.0 1.0 1.0 1.0
  setAmbientLight g3ds white
  light <- pointLight g3ds white (Vec3 10.0 10.0 20.0)
  loadGuiScheme guis "hg3d.scheme"
  cuboLayout <- loadGuiLayoutFromFile guis "cuboid2.layout" ""
  addGuiElToDisplay guis cuboLayout
  
  let (redMesh, greenMesh, blueMesh) = createMeshes
  (sphereRed, sphereGreen) <- createSpheres g3ds redMesh greenMesh
  
  rE <- veloEntity "s1" sphereRed (Vec3 (-60.0) 0.0 0.0)
  gE <- veloEntity "s2" sphereGreen (Vec3 60.0 0.0 0.0)
  
  lines <- create3dLines g3ds (oneAndOnlyLevel !! 0)
  cubes <- createCubes g3ds blueMesh
  cbsE <- mapM (\(i, o) -> veloEntity ("cube" ++ (show i)) o (cubeStartPos i)) (zip [0..] cubes)
  
  combined <- object3DFromObjects g3ds $ [sphereRed, sphereGreen] ++ lines ++ cubes
  cE <- rotEntity "combi" combined unitU
  
  sounds <- createSounds
  
  -- create the systems
  let x = (Vec3 1.0 0.0 0.0)
  let locSysW = createSystem locationSystem
  let oriSysW = createSystem orientationSystem
  let velSysW = createSystem velocitySystem
  
  -- create game logic wire
  let gameWire re ge ce = proc inCmd -> do
        rec
          evtMove <- getStepsFromKeyW ce -< ()
          evtNewLevel <- now -< oneAndOnlyLevel
          evtNextLevel' <- delay (U.Event ()) -< evtNextLevel
          evtCuboid <- buildCuboidW rE gE cbsE . runWithOneEvtW . runLevelW -< (evtNewLevel, evtNextLevel')
          evtNextLevel <- runCuboidW sounds re -< (evtCuboid, evtMove)
                          
        (rotate ce x 1.0) . keyW KeyW -< inCmd             
        (rotate ce x (-1.0)) . keyW KeyS -< inCmd          
        outVal <- cmdFifo . locSysW . velSysW . oriSysW -< inCmd      
        
        returnA -< outVal                                         
        
  let startCmd = CmdArr [
        (CmdAddEntity rE),
        (CmdAddEntity gE),
        (CmdAddEntity cE),
        (CmdArr (map CmdAddEntity cbsE))
        ]
    
  renderLoop g3ds guis clockSession_ (gameWire rE gE cE) (U.Event startCmd)
  exitHGamer3D g3ds guis
  return ()



