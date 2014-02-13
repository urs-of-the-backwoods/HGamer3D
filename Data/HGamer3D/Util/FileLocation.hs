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

-- Util.hs

-- | Utility to provide file location information to other parts of HGamer3D
module HGamer3D.Util.FileLocation

(
  
        -- * Directories relative to the user-related application directory for HGamer3D
        getAppMediaDirectory,
        getAppConfigDirectory,
        getAppLibDirectory,
        
        -- * Directories relative to the executable (prog path)
        getExeMediaDirectory,
        getExeConfigDirectory,
        getExeLibDirectory,
        
        -- * General utilities for path handling and finding files
        osSep,
        createDir,
        findFileInDirs
)

where

import Control.Monad
import System.Directory
import System.FilePath
import System.Environment.FindBin

_getHG3DDirectory subdir = do
  appdir <- getAppUserDataDirectory "HGamer3D" 
  let ndir = appdir ++ [pathSeparator] ++ subdir
  createDirectoryIfMissing True ndir
  return ndir
  
-- | path of media, relative to user app dir for HGamer3D
getAppMediaDirectory  = _getHG3DDirectory "media"

-- | path of configuration, relative to user app dir for HGamer3D
getAppConfigDirectory  = _getHG3DDirectory "config"

-- | path of libraries, relative to user app dir for HGamer3D
getAppLibDirectory = _getHG3DDirectory "lib"
  
_getBinDir subdir = do
  bdir <- getProgPath
  let ndir = bdir ++ [pathSeparator] ++ ".HGamer3D" ++ [pathSeparator] ++ subdir
  return ndir

-- | path of media, relative to executable
getExeMediaDirectory = _getBinDir "media"

-- | path of configuration, relative to executable
getExeConfigDirectory = _getBinDir "config"

-- | path of libraries, relative to executable
getExeLibDirectory = _getBinDir "lib"

-- | path separator for the filesystem
osSep = [pathSeparator]

-- | create a directory
createDir dir = createDirectoryIfMissing True dir

-- | find a file by searching in multiple directories
findFileInDirs filename listOfDirs = do
  let files = fmap (\d -> d ++ [pathSeparator] ++ filename) listOfDirs
  res <- filterM doesFileExist files
  if length res > 0 then return $ Just (res !! 0) else return Nothing