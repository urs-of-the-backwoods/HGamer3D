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

-- Engine.hs

-- | Game-Loop and internals of Base API implementation

module HGamer3D.APIs.Base.Engine.Engine (

        -- * utility functions
	getUniqueName,
	mapFunctionToTag,
	getFunctionFromTag,
	getTimeMS,
        
        -- * basic functions, to constitute running system
--	initCommonSystem,
	runMHGamer3D,
	initHGamer3D,
	renderLoop
        
        -- * usage of game loop
        -- $intro      

) 

where


import GHC.Ptr

import HGamer3D.Data.Colour
import HGamer3D.Data.Vector
import HGamer3D.Data.Angle
import HGamer3D.Data.HG3DClass

import HGamer3D.Bindings.Ogre.ClassRoot as Root
import HGamer3D.Bindings.Ogre.ClassHG3DMessagePump as MessagePump
import HGamer3D.Bindings.Ogre.ClassRenderWindow as RenderWindow
import HGamer3D.Bindings.Ogre.ClassWindowUtilsHG3D as WindowUtils
import HGamer3D.Bindings.CEGUI.ClassSystem as CEGUISystem
import HGamer3D.Bindings.CEGUI.EnumMouseButton as CEGUIButton
import HGamer3D.Bindings.CEGUI.ClassHG3DEventController as HG3DEventController

import HGamer3D.APIs.Base.Engine.Types
import qualified HGamer3D.APIs.Base.InputSystem.InputSystem as IS

import HGamer3D.APIs.Base.Graphics3D.EngineHelper
import HGamer3D.APIs.Base.GUI.EngineHelper
import HGamer3D.APIs.Base.Network.EngineHelper
import HGamer3D.APIs.Base.Physics.EngineHelper

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Reader
import Control.Monad.State
import Control.Concurrent
import Data.List

import System.Win32.Process
import qualified Data.Text as T
import System.Directory
import System.Environment (getArgs)
import qualified Data.Map as Map
import System.Win32.Time
import System.Environment.FindBin
import System.Win32.Registry

import Data.List.Split


-- | get unique identifiers, by appending a unique integer to a prefix
getUniqueName :: String -> MHGamer3D String
getUniqueName prefix = do 
    enstate <- lift get
    let HG3DEngineState (x:xs) evtmap = enstate
    lift $ put (HG3DEngineState xs evtmap)
    return (prefix ++ (show x))
 
-- |event function mapping, map a function to a tag (a string identifier)
mapFunctionToTag :: String -> EventFunction -> MHGamer3D ()
mapFunctionToTag tag function = do
    enstate <- lift get
    let HG3DEngineState un evtmap  = enstate
    let evtmap2 = Map.insert tag function evtmap
    lift $ put (HG3DEngineState un evtmap2)
    return ()

-- |event function mapping, return the function of the tag (string identifier)
getFunctionFromTag :: String -> MHGamer3D (Maybe EventFunction)
getFunctionFromTag tag = do
    enstate <- lift get
    let eventMap = esEventMap enstate
    let fr = Map.lookup tag eventMap
    return fr

-- function, which finds installation path of HG3D
--

-- assumes very strict versioning info in version.cfg, 3rd line will have version without dots!
-- version.cfg will be located in "media/engine"

readVersion path = do 
	text <- readFile (path ++ "/media/engine/version.cfg")
	let ver = (splitOn "=" ((splitOn "\n" text)!!2))!!1
	return ver
        

-- function, which ignores all behind a specific string
ignoreTextAfterIndicator :: String -> String -> String
ignoreTextAfterIndicator text indicator = case text of 
                [] -> []
                (x:xs) -> if (indicator `Data.List.isPrefixOf` text) then [] else (x : (ignoreTextAfterIndicator xs indicator))

getHG3DPath :: IO (String, String)
getHG3DPath = do
	progpath2 <- getProgPath
        -- fix error of getProgPath in ghci, cut of line at "\-l..."
        let progpath = ignoreTextAfterIndicator progpath2 "\\-l"
	-- read version file
	v <- readVersion progpath
	key <- regCreateKey hKEY_LOCAL_MACHINE ("SOFTWARE\\Microsoft\\Windows\\CurrentVersion\\App Paths\\hg3dpath" ++ v ++ ".exe")
	binpath <- regQueryValue key (Just "Path")
	-- remove "/bin" from path
	let path = take (length binpath - 4) binpath
	return (path, progpath)

-- | internal function, to initialize common system record, not used by end-users
initCommonSystem :: IO CommonSystem
initCommonSystem = do
	(csHG3DPath, csProgPath) <- getHG3DPath
	return (CommonSystem csHG3DPath csProgPath)

-- Milliseconds Timescale

-- | get current game time in milliseconds
getTimeMS :: MHGamer3D (TimeMS)
getTimeMS = do 
	fr <- liftIO $ queryPerformanceFrequency
	wt <- liftIO $ queryPerformanceCounter
	return (TimeMS $ fromIntegral (wt * 1000 `div`  fr))
	
-- | run a HGamer3D monad function within the states of the game
runMHGamer3D :: (HG3DReaderState, HG3DEngineState) -- ^ initial state, taken from initMHGamer3D 
                -> MHGamer3D a -- ^ the action, which should be run in the monad
                -> IO (a, (HG3DReaderState, HG3DEngineState)) -- ^ the output value and updated state of the monad
runMHGamer3D (readerstate, enginestate) action = do
	(actionResult, newEnginestate) <- runStateT (runReaderT action readerstate ) enginestate
	return (actionResult, (readerstate, newEnginestate))

data MouseState = MouseUp | MouseDown deriving (Eq)

data TimeMouseState = TimeMouseState {
	tmsMouse::MouseState,
	tmsTimeMS::TimeMS
	}
	
initTimeMouseState :: MHGamer3D (TimeMouseState)
initTimeMouseState = do
	tms <- getTimeMS
	leftButton <- IS.isMouseButtonPressed IS.MouseButtonLeft
	let ms = if leftButton then MouseUp else MouseDown
	return (TimeMouseState ms tms)

-- | initialize the HGamer3D system
initHGamer3D :: String -- ^ name of the window (shown in main title bar)
                -> IO (HG3DReaderState, HG3DEngineState) -- ^ the state value needed for further invocation of HGamer3D code through the monad.
initHGamer3D windowName = do

	-- init Commons configs
	cs <- initCommonSystem
	
	-- get flags from program arguments
	let hg3dpath = (csHG3DPath cs) 
	args <- getArgs
	let fConfig = foldl (||) False $ map (\arg -> if arg == "--config" then True else False) args
	let fDX = foldl (||) False $ map (\arg -> if arg == "--directx" then True else False) args
	let fLog = foldl (||) False $ map (\arg -> if arg == "--logging" then True else False) args
	
	-- init Graphics3D configs and engine
	g3s <- initGraphics3D windowName "OctreeSceneManager" hg3dpath fConfig fDX fLog
	
	-- init GUI configs and engine
	gui <- initGUIEngine fLog
	
	-- init Network engine
	network <- initNetworkEngine
	
	-- init Phyiscs engine
	physics <- initPhysicsEngine
	
        let eventmap = Map.fromList ([]::[(String, EventFunction)])
	let enginestate = HG3DEngineState [1..] eventmap
	let readerstate = HG3DReaderState cs g3s gui network physics
	
	return (readerstate, enginestate)
	
-- renderStep a :: TimeMS -> a -> MHGamer3D (Bool, a)
-- this function needs to be defined in 

renderInternalStep :: Int -> TimeMouseState -> MHGamer3D (Bool, TimeMouseState)
renderInternalStep frameRate (TimeMouseState lastMouseState (TimeMS lastTime)) = do
	rs <- ask
	let cs = commonSystem rs
	let g3s = graphics3DSystem rs
	let gui = guiSystem rs
	-- adapt to framerate, while still doing messagePump
	(TimeMS time) <- getTimeMS
	let delta = time - lastTime
	let waitTimeMS = (1000.0 / (fromIntegral frameRate) ) - (fromIntegral delta) 
	time <- if waitTimeMS > 0.0 then do
				liftIO $ sleep (round waitTimeMS)
				(TimeMS time) <- getTimeMS
				return (time)
			else
				return (time)
	-- adapt
	let messagePump = g3sMessagePump g3s
	liftIO $ MessagePump.messagePump messagePump
	closed <- liftIO $ RenderWindow.isClosed (g3sRenderWindow g3s)
	if (closed) then
		return (False, (TimeMouseState lastMouseState (TimeMS time)) )
		else do
			-- here comes the things, we need to do each time
			-- 
			let delta2 = (fromIntegral (time - lastTime)) * 1000.0
			-- mouse state injection
			leftButton <- IS.isMouseButtonPressed IS.MouseButtonLeft
			let ms = if leftButton then MouseDown else MouseUp
			if lastMouseState == MouseUp && ms == MouseDown then do
				liftIO $ CEGUISystem.injectMouseButtonDown  (guiGUI gui) CEGUIButton.MouseLeftButton
				return ()
				else do
					return ()
			if lastMouseState == MouseDown && ms == MouseUp then do
				liftIO $ CEGUISystem.injectMouseButtonUp (guiGUI gui) CEGUIButton.MouseLeftButton
				return ()
				else do
					return ()
			-- mouse position injection
			(xm, ym) <- IS.getMousePosition
			leftButton <- IS.isMouseButtonPressed IS.MouseButtonLeft
			(width, height, colorDepth, left, top) <- liftIO $ RenderWindow.getMetrics (g3sRenderWindow g3s)
			(topT, bottomT, leftT, rightT) <- liftIO $ WindowUtils.getWindowTopLeft (g3sRenderWindow g3s) 
			let offLeft = (rightT - leftT - width) `div` 2
			let offRight = bottomT - topT - height - offLeft
			-- key press injection
			keypressInject
			
			let mouseX = xm - left - offLeft
			let mouseY = ym - top - offRight
			liftIO $ CEGUISystem.injectMousePosition (guiGUI gui) (fromIntegral mouseX) (fromIntegral mouseY)
			-- time pulse injection
			liftIO $ CEGUISystem.injectTimePulse (guiGUI gui) delta2
			-- display 3D and GUI
			liftIO $ Root.renderOneFrame (g3sRoot g3s)
			liftIO $ CEGUISystem.renderGUI (guiGUI gui)
			
			return (True, (TimeMouseState ms (TimeMS time)) )


renderInternalLoop :: Int -> TimeMouseState -> gamestateType -> (TimeMS -> gamestateType -> MHGamer3D (Bool, gamestateType)) -> MHGamer3D ()
renderInternalLoop frameRate rsold gamestate renderStep = do
	let (TimeMouseState ms (TimeMS timeold)) = rsold
	getEventsFromGui 
	(flagStep, rs) <- renderInternalStep frameRate rsold
	let (TimeMouseState ms (TimeMS time)) = rs
	let delta = (TimeMS (time - timeold))
	(flagLoop, gsnew) <- renderStep delta gamestate
	if (flagStep && flagLoop) then do
		renderInternalLoop frameRate rs gsnew renderStep
		else return ()
			
-- | the main loop, call this at end of your world preparation, to start game loop
renderLoop :: Int -- ^ the frame rate, you want to achieve, frames per second
              -> worldState -- ^ a type of your choice for the world state
              -> (TimeMS -> worldState -> MHGamer3D (Bool, worldState)) -- ^ the renderStep function, a function which gets called each step, the bool value indicates if world should run further (True) or if world should stop (False).
              -> MHGamer3D ()
renderLoop frameRate gamestate renderStep = do
	rs <- initTimeMouseState
	renderInternalLoop frameRate rs gamestate renderStep

-- event loop

getEventsFromGui :: MHGamer3D ()
getEventsFromGui = do
	rs <- ask
	let cs = commonSystem rs
	let g3s = graphics3DSystem rs
	let gui = guiSystem rs
	let eventController = guiEventController gui
	processEvents <- liftIO $ HG3DEventController.eventsAvailable eventController
	if processEvents then do
		(name, sender, window) <- liftIO $ HG3DEventController.popEvent eventController
		let evt = GUIEvent name sender window
		evtfunc <- getFunctionFromTag name 
		case evtfunc of
			Just func -> do
				func evt
				return ()
			Nothing -> return ()
		getEventsFromGui 
		return ()
		else do
			return ()
	return ()

{-$intro
use as in the following code example:
first define a renderStep function

@
 renderStep :: TimeMS -> Gameworld -> MHGamer3D (Bool, Gameworld)
 renderStep (TimeMS time) cube = do
   yaw3D cube (Rad 0.005) 
   roll3D cube (Rad 0.002)
   return (True, cube)
@ 

in the main program, initialize your objects, and at the end call the game loop

@
 main3DProgram = do
   \-- do some more init here: lights, objects, ...
   renderLoop 40 cubeBlue renderStep
   return ()
@

in the main program, just run the main init routine and the main program in the HGamer3D monad

@
 main = do
	hg <- initHGamer3D \"HGamer3D - Getting Started Example\" 
	(l, hg) <- runMHGamer3D hg main3DProgram 
	return ()
@

-}

