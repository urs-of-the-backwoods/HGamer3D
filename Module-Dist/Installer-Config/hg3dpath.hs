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


-- hg3dpath.hs

module Main where

import qualified Data.Text as T
import System.Win32.Registry
import Data.List.Split
import System.Environment.FindBin

-- assumes very strict versioning info in version.cfg, 3rd line will have version without dots!
-- version.cfg will be located in "media/engine"

readVersion path = do 
	text <- readFile (path ++ "\\version.cfg")
	let ver = (splitOn "=" ((splitOn "\n" text)!!2))!!1
	return ver
        
getHG3DPath :: IO String
getHG3DPath = do
	progpath <- getProgPath
	-- read version file
	v <- readVersion progpath
	key <- regCreateKey hKEY_LOCAL_MACHINE ("SOFTWARE\\Microsoft\\Windows\\CurrentVersion\\App Paths\\hg3dpath" ++ v ++ ".exe")
	path <- regQueryValue key (Just "Path")
	return path

main = do
	p <- getHG3DPath
	putStr p
	return ()
	


