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

-- WireAPI.hs

-- |HGamer3D FRP API: an arrowized functional reactive programming API, based on the netwire library (<http://hackage.haskell.org/package/netwire>).

module HGamer3D.APIs.FRP

(
        -- * Types
        module HGamer3D.APIs.FRP.Types,
  
        -- * Main FRP API functionality
	module HGamer3D.APIs.FRP.GUI,
        module HGamer3D.APIs.FRP.Graphics3D,
        module HGamer3D.APIs.FRP.InputSystem,
        module HGamer3D.APIs.FRP.Network,

        -- * re-exported Base API
	module HGamer3D.APIs.Base
)

where

import HGamer3D.APIs.Base
import HGamer3D.APIs.FRP.GUI
import HGamer3D.APIs.FRP.Graphics3D
import HGamer3D.APIs.FRP.InputSystem
import HGamer3D.APIs.FRP.Network
import HGamer3D.APIs.FRP.Types
