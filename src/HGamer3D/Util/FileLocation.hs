-- This source file is part of HGamer3D
-- (A project to enable 3D game development in Haskell)
-- For the latest info, see http://www.hgamer3d.org
--
-- (c) 2011 - 2015 Peter Althainz
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
        getAppDirectory,
        getMediaDirectory,
        getConfigDirectory,
        
        -- * General utilities for path handling and finding files
        pathSeparator
)

where

import Control.Monad
import Control.Exception
import System.FilePath
import System.Directory

-- | path of subdirectory within platform specific application directories
getAppDirectory = getAppUserDataDirectory

-- | path of HGamer3D Media Directory
getMediaDirectory = getAppDirectory ("HGamer3D" ++ [pathSeparator] ++ "media")

-- | path of HGamer3D configuration directory 
getConfigDirectory = getAppDirectory ("HGamer3D" ++ [pathSeparator] ++ "config")

