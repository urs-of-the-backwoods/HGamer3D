-- This source file is part of HGamer3D
-- (A project to enable 3D game development in Haskell)
-- For the latest info, see http://www.hgamer3d.org
--
-- (c) 2011-2013 Peter Althainz
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


-- InputSystem-Joystick.hs

module Main where

import HGamer3D.InputSystem.BaseAPI
import Control.Monad


-- helper loop, to show axes values

getJoysticks = do
	updateJoystickStatus
	js <- getConnectedJoysticks
	return js

showAxesValues jn axes = do
	updateJoystickStatus
	vals <- mapM (\a -> do 
		getJoystickAxisPosition jn a) axes
	putStrLn $ "Joystick Positions: " ++ (show vals)
	showAxesValues jn axes
	return ()
		
getJoystickProperties jn = do
	jbs <- getJoystickButtons jn
	jax <- getJoystickAxes jn
	return (jbs, jax)
	


-- start main Joystick tests

main = do

	-- detect, which Joysticks are connected
	js <- getJoysticks
	putStrLn "The following Joysticks are connected:"
	mapM (\j -> do
		putStrLn (show j))  js
		
	-- choose your Joystick for following tests

	putStrLn "Choose one (only type the number):"
	line <- readLn
	let jn = (fromIntegral (toInteger line))::Int
	let joystick = js !! jn
	
	if (elem joystick js) then do
		putStrLn $  (show jn) ++ " selected"
		else do
			putStrLn $  "no valid Joystick selected"

-- now, if proper Joystick selected, run tests

	if (elem joystick js) then do
	
		-- show properties of Joystick
		(bs, axes) <- (getJoystickProperties joystick)
		putStrLn "Your Joystick has the following properties:"
		mapM (\b -> do
			putStrLn $ show b ) bs
		mapM (\a -> do
			putStrLn $ show a) axes
		

		-- show values of axes, until button one is pressed
		
		putStrLn "After Enter, show values of all Axes"
		getChar
		
		x <- showAxesValues joystick axes
			
		return ()

		else
			return ()
			
