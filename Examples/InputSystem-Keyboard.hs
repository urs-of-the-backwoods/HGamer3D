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


-- InputSystem-Keyboard.hs

module Main where

import HGamer3D.APIs.Base
import Control.Monad
import Control.Monad.Trans


loop = do

	a <- isKeyPressed KeyA
	s <- isKeyPressed KeyS
	d <- isKeyPressed KeyD
	f <- isKeyPressed KeyF
	
	liftIO $ putStrLn ( "Keys pressed in asdf:" ++ (show [a,s,d,f]))
	
	loop

		
main = do
	-- init hgamer3d
	hg <- initHGamer3D "HGamer3D - Keyboard Example" 
	
	
	putStrLn "This test shows keys pressed in ASDF keys, press a key"
	getChar

	-- loop keyboard input
	(x, hg) <- runMHGamer3D hg loop
	return ()
