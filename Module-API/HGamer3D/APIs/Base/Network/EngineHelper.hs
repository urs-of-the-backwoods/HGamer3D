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

-- EngineHelper.hs

-- | Internal functions of the network functionality, used by the Base API.
module HGamer3D.APIs.Base.Network.EngineHelper

(
		initNetworkEngine
)

where

import GHC.Ptr

import HGamer3D.Data.HG3DClass
import HGamer3D.Data.Vector

import HGamer3D.Bindings.Enet.ClassPtr
import HGamer3D.Bindings.Enet.Utils

import HGamer3D.Bindings.Enet.ClassEnet as Enet

import HGamer3D.APIs.Base.Engine.Types

import Control.Monad.Trans
import Control.Monad.Reader


-- | intializes the network engine, used internally in the Base API.
initNetworkEngine :: IO (NetworkSystem)
initNetworkEngine = do
	enet <- Enet.new
	let ns = (NetworkSystem enet)
	return ns
