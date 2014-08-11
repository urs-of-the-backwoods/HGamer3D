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


-- InputSystem-Mouse.hs

module Main where

import HGamer3D.InputSystem.BaseAPI
import Control.Monad

loop = do
	l <- isMouseButtonPressed MouseButtonLeft
	r <- isMouseButtonPressed MouseButtonRight
	(x, y) <- getMousePosition 
	
	putStrLn ( "Mouse (left, right, position:" ++ (show (l,r,x,y)))
	
	loop
		
main = do
	putStrLn "This test shows mouse buttons and position, press a key"
	getChar

	-- loop mouse input
	x <- loop
	return ()
