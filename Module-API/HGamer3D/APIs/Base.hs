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

-- BaseAPI.hs

-- |HGamer3D Base API: basic Haskell API for HGamer3D, based on the low-level bindings, with moderate abstraction on top of it.

module HGamer3D.APIs.Base

(
        -- * Data definitions, re-exported
	module HGamer3D.Data.Colour,
	module HGamer3D.Data.Vector,
	module HGamer3D.Data.Angle,

        -- * Main API functionality, split into sub modules
	module HGamer3D.APIs.Base.Audio.Audio,
	module HGamer3D.APIs.Base.InputSystem.InputSystem,
	module HGamer3D.APIs.Base.Graphics3D.Basic3D,
	module HGamer3D.APIs.Base.Graphics3D.Light,
	module HGamer3D.APIs.Base.Graphics3D.Object3D,
	module HGamer3D.APIs.Base.Graphics3D.PlatonObjects,
	module HGamer3D.APIs.Base.GUI.BasicGUI,
	module HGamer3D.APIs.Base.Physics.Physics,
	module HGamer3D.APIs.Base.Network.Network,
	
        -- * Game-Loop and internal functionality
	module HGamer3D.APIs.Base.Engine.Types,
	module HGamer3D.APIs.Base.Engine.Engine
)

where

import HGamer3D.Data.Colour
import HGamer3D.Data.Vector
import HGamer3D.Data.Angle

import HGamer3D.APIs.Base.Audio.Audio
import HGamer3D.APIs.Base.InputSystem.InputSystem
import HGamer3D.APIs.Base.Engine.Types
import HGamer3D.APIs.Base.Engine.Engine

import HGamer3D.APIs.Base.Graphics3D.Basic3D
import HGamer3D.APIs.Base.Graphics3D.Light
import HGamer3D.APIs.Base.Graphics3D.Object3D
import HGamer3D.APIs.Base.Graphics3D.PlatonObjects

import qualified Data.Map as Map

import HGamer3D.APIs.Base.GUI.BasicGUI
import HGamer3D.APIs.Base.Physics.Physics
import HGamer3D.APIs.Base.Network.Network

import Control.Monad.Trans (liftIO)

