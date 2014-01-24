-- This source file is part of HGamer3D
-- (A project to enable 3D game development in Haskell)
-- For the latest info, see http://www.althainz.de/HGamer3D.html
--
-- (c) 2011 Peter Althainz
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


-- YWing-Flight.hs

module Main where

import HGamer3D.APIs.Base
import Control.Monad
import Control.Monad.Trans
import Data.IORef

-- some basic vectors
xv = Vec3 1.0 0.0 0.0
yv = Vec3 0.0 1.0 0.0
zv = Vec3 0.0 0.0 1.0
v0 = Vec3 0.0 0.0 0.0

-- functions, to rotate on axis, relative to object
rotRelativeToObjectAxis :: Object3D -> Vec3 -> Float -> MHGamer3D ()
rotRelativeToObjectAxis object axis val = do
	qob <- orientation3D object
	let odir = actU qob axis
	let qrot = rotU odir val
	let nrot = qrot .*. qob
	orientationTo3D object nrot
	return ()
moveYaw object val = rotRelativeToObjectAxis object yv val
moveRoll object val = rotRelativeToObjectAxis object zv val
movePitch object val = rotRelativeToObjectAxis object xv val

-- function, to move into direction of flight
moveTrans :: Object3D -> Float -> MHGamer3D ()
moveTrans object val = do
	quat <- orientation3D object
	-- this points towards nose
	let vdir = actU quat zv
	translate3D object ( val *& vdir)
	return ()

-- Mover, encapsulation of moving by input source	
data Mover = Mover Float EnumJoystickAxis (Object3D -> Float -> MHGamer3D ())

doMover :: Object3D -> Mover -> Joystick -> MHGamer3D ()
doMover object (Mover val axis mover) joystick = do
	updateJoystickStatus
	valAction <- getJoystickAxisPosition joystick axis
	if (abs valAction) > 2 then do
		mover object (valAction * val)
		return ()
		else return ()

createMover :: EnumJoystickAxis -> Float -> (Object3D -> Float -> MHGamer3D ()) -> Mover
createMover axis value movef = (Mover value axis movef)

createMovers :: [Mover]
createMovers  = map (\(axis, factor, movef) -> createMover axis factor movef) [

		(JoystickAxisX, -0.00005, moveYaw),
		(JoystickAxisY, -0.00005, movePitch),
		(JoystickAxisR, -0.00005, moveRoll),
		(JoystickAxisZ, -0.15, moveTrans) ]

-- camera position management			]
setNewCameraPosition :: Object3D -> [(Vec3, UnitQuaternion)] -> MHGamer3D ()
setNewCameraPosition ywing newhist = do

		wingpos <- position3D ywing
		let (pos, quat) = newhist !! 20
		-- this points towards nose
		let zdir = actU quat zv
		let ydir = actU quat yv
		let newcampos = ( (-50.0) *& zdir) &+ pos &+ (20.0 *& ydir) 
	
		c <- getCamera
		positionTo3D c newcampos
		directionTo3D c (wingpos &- newcampos)
		return ()


-- Joystick selection handling

initJoystickState outtext = do
        ref <- liftIO $ newIORef (Nothing::Maybe Joystick)
	setGuiElProperty outtext "Text" "Press C to configure!"
	return ref
	
type StepFuncStateType = ( GUIElement, [Mover], Object3D, Object3D, [(Vec3, UnitQuaternion)], IORef (Maybe Joystick))

selectJoystick :: GUIElement -> GUIElement -> IORef (Maybe Joystick) -> EventFunction
selectJoystick comboWin confWin selRef event = do
	-- define js
	txt <- getGuiElProperty comboWin "Text"
--	liftIO $ print ("selected text: " ++ txt)
	let js = snd ((filter (\(t,j) -> t == txt) [ ("Joystick " ++ (show x), Joystick x)| x <- [0..20]] ) !! 0)
--	liftIO $ print ("selected js: " ++ (show js))
	-- setGuiElProperty outtext "Text" "Joystick selected!"
	setGuiElProperty confWin "Visible" "False"
        liftIO $ writeIORef selRef (Just js)
	return ()
	
	
selectVolume :: EventFunction
selectVolume event = do
	case event of
		GUIEvent name sender window -> do
			val <- getGuiElProperty (GUIElement window) "CurrentValue"
--			liftIO $ print val
			setAudioMainVolume (read val)
			return ()
		_ -> return ()
			
	
setupVolumeSlider window = do

	slider <- findChildGuiElRecursive window "SliderMusicVolume"
	
	case slider of
		Just sliderWin -> do
			setGuiElProperty sliderWin "CurrentValue" "100.0"
			mapGuiElEventToFunction sliderWin "ValueChanged" selectVolume
			return ()
		Nothing -> do
			return ()
		
	
setupJoystickSelectionBox window ref = do

	combo <- findChildGuiElRecursive window "ComboboxJoystick"
	case combo of
		Just comboWin -> do
			addGuiElToDisplay window
			
			-- for all joysticks found
			let ns = [ x | x <- [0..10]]
			updateJoystickStatus
			js <- filterM (\x -> do
				c <- isJoystickConnected (Joystick x)
				return c ) ns
				
			mapM (\joyn -> do
				let joy = (Joystick joyn)
				comboboxAddText comboWin ("Joystick " ++ (show joyn))
				) js
				
			mapGuiElEventToFunction comboWin "ListSelectionAccepted" (selectJoystick comboWin window ref)
			return ()
		Nothing -> do
			return ()
		



-- function called during each frame
stepFunc :: TimeMS -> StepFuncStateType -> MHGamer3D (Bool, StepFuncStateType)		
stepFunc  time (configwin, movers, world, ywing, history, joyState) = do

	-- check if F1 key is pressed for selection menue
	flagMenu <- isKeyPressed KeyC
	if flagMenu then do
		setGuiElProperty configwin "Visible" "True"
		return (True, (configwin, movers, world, ywing, history, joyState))
		else do
		

			-- do moves, only if Joystick selected
                        jState <- liftIO $ readIORef joyState
                        case jState of
                          Just js -> do
				sequence $ map ( \m -> doMover ywing m js) movers

				-- get new position and orientation
				q <- orientation3D ywing
				pos <- position3D ywing
				let newhist = history ++ [(pos, q)]
				

				-- work on history
				if length newhist > 30 then do
					let newhist2 = drop 1 newhist
					setNewCameraPosition ywing newhist
					return (True, (configwin, movers, world, ywing, newhist2, joyState))
					else return (True, (configwin, movers, world, ywing, newhist, joyState))
					
                          Nothing -> return (True, (configwin, movers, world, ywing, history, joyState))


-- build the world in 3D

createCubeTemplates = do

	-- create ring templates
	let col = Colour 1.0 1.0 1.0 1.0
	let red = Colour 1.0 0.0 0.0 1.0
	let yellow = Colour 1.0 1.0 0.0 1.0
	blueT <- createColouredCubeMesh (NamedMaterial "YWing/Blue50") col
	redT <-  createColouredCubeMesh  (NamedMaterial "BaseWhiteNoLighting") red
	yellowT <-  createColouredCubeMesh  (NamedMaterial "BaseWhiteNoLighting") yellow
	return (blueT, redT, yellowT)
	

createGates thickness freespace depth cubeT r = do

	-- parameters horizontal bars
	let hl = freespace + thickness  -- length horizontal bar
	let hw = thickness/2.0		    -- width horizontal bar
	let hpt = Vec3 0.0 (freespace + thickness/2.0) 0.0 -- horizontal bar, position top
	let hpb = Vec3 0.0 (-1.0 * (freespace + thickness/2.0)) 0.0 -- horizontal bar, position bottom
	
	-- create top and bottom cube
	top <- createObject3DFromMesh cubeT
	bottom <- createObject3DFromMesh cubeT
	scaleTo3D top (Vec3 hl hw depth)
	scaleTo3D bottom (Vec3 hl hw depth)
	positionTo3D top hpt
	positionTo3D bottom hpb
	
	-- parameters vertical bars
	let vl = freespace						-- vertical bar, length
	let vw = thickness/2.0						-- vertical bar, width
	let vpr = Vec3 (freespace + thickness/2.0) 0.0 0.0    -- vertical bar, position right
	let vpl = Vec3 (-1.0 * (freespace + thickness/2.0)) 0.0 0.0
	
	-- create left and right cube
	left <- createObject3DFromMesh cubeT
	right <- createObject3DFromMesh cubeT
	scaleTo3D left (Vec3 vw vl depth)
	scaleTo3D right (Vec3 vw vl depth)
	positionTo3D left vpl
	positionTo3D right vpr
	
	gate <- combineObjects [top, bottom, left, right]
	translate3D gate (Vec3 0.0 0.0 r) 
	
	return gate
	

createLines space cubeAT cubeBT r = do

	let x = 0.0
	let y = 0.0
	let z = 0.0
	
	-- yellow and red lines along the way
	let a1 = Vec3 (x + space) y z
	let a2 = Vec3 (x - space) y z
	let a3 = Vec3 x (y + space) z
	let a4 = Vec3 x (y - space) z
	let ascale = Vec3 0.5 0.5 5.0
	
	red1 <- createObject3DFromMesh cubeAT
	positionTo3D red1 a1
	scaleTo3D red1 ascale

	yellow1 <- createObject3DFromMesh cubeBT
	positionTo3D yellow1 a3
	scaleTo3D yellow1 ascale

	red2 <- createObject3DFromMesh cubeAT
	positionTo3D red2 a2
	scaleTo3D red2 ascale

	yellow2 <- createObject3DFromMesh cubeBT
	positionTo3D yellow2 a4
	scaleTo3D yellow2 ascale

	-- combine all and give back correct i for further processing
	lines <- combineObjects [red1, red2, yellow1, yellow2]
	translate3D lines (Vec3 0.0 0.0 r) 

	
	return lines
	
	
-- function calculating flow of gates in the world
modifyPos x object = do

	translate3D object (Vec3 (100*(sin (x/500.0))) (100*(cos (x/500.0))) 0.0) 
	return object


createLineCubes = do

	(blueT, redT, yellowT) <- createCubeTemplates
    
	gates <- mapM (\r -> createGates 20.0 100.0 10.0 blueT r >>= modifyPos r ) (map (\x -> 100.0 * x) [1..1000])
	lines <- mapM (\r -> createLines 60.0 redT yellowT r >>= modifyPos r ) (map (\x -> 20.0 * x) [1..5000])
	
	lc <- combineObjects (gates ++ lines)
	return lc



-- main 3d program
main3DProgram = do
        -- book: media locations
	-- add media locations
	addResourceLocationGUI "media\\gui\\layout" "Layout"
	addResourceLocationMedia "media\\materials"
	addResourceZipfileMedia "media\\ywing.zip"
	finalizeResourceLocations
        -- book: end
	
	c <- getCamera

	-- camera position
	let pos = Vec3 20.0 20.0 (-100.0)
	positionTo3D c pos
	let at = Vec3 0.0 0.0 (300.0)
	directionTo3D c at
	
	-- load ywing
	ywing <- loadMesh "ywing.mesh"
	scaleTo3D ywing (Vec3 0.05 0.05 0.05)

	lc <- createLineCubes
	world <- combineObjects ([ywing] ++ [lc])

	-- define light
	let col = Colour 1.0 1.0 1.0 1.0
	light <- createPointlight col (Vec3 20.0 80.0 50.0)
	setAmbientLight col

	-- GUI Code starts here, display hg3d logo
	loadGuiScheme "hg3d.scheme"
	logo <- loadGuiLayoutFromFile "hgamer3d.layout" ""
	addGuiElToDisplay logo
	
	mu <- createMusic "New Friendly.wav"
	case mu of
		Just audio -> do
			playAudioSource audio
			return (Just audio)
		Nothing -> do
			return Nothing
			
		-- create Movers
	let movers = createMovers

	configwindow <- loadGuiLayoutFromFile "configurator.layout" ""
	sttext <- loadGuiLayoutFromFile "statictext.layout" ""
	outtext <- findChildGuiElRecursive sttext "OutputTextBox"
	
	case outtext of
		Just outtext -> do
			addGuiElToDisplay outtext
			joyState <- initJoystickState outtext
			time <- getTimeMS
			
			setupJoystickSelectionBox configwindow joyState
			setupVolumeSlider configwindow
			
			renderLoop 100 (configwindow, movers, world, ywing, [], joyState) stepFunc
			return ()
		Nothing -> do
			return ()

	return ()



main = do
	-- initialize
	hg <- initHGamer3D "HGamer3D Example - YWing Flight" 
	(l, hg) <- runMHGamer3D hg main3DProgram 
	return ()
	
	
		
	

