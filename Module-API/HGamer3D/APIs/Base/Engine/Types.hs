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

-- Types.hs

-- |Types of the Base API.

module HGamer3D.APIs.Base.Engine.Types (

        -- * records for sub system objects from low-level API
	CommonSystem (..),
	Graphics3DSystem (..),
	GUISystem (..),
	NetworkSystem (..),
	PhysicsSystem (..),
	
        -- * the main HGamer3D monad
	HG3DReaderState (..),
	HG3DEngineState (..),
	MHGamer3D,
        
        -- * Events
	Event (..),
	EventFunction,
	EventMap,
		
        -- * Time
	TimeMS (..),
)

where 

import HGamer3D.Data.HG3DClass
import Control.Monad.Reader
import Control.Monad.State
import qualified Data.Map as Map

-- | General information, needed in all sub-systems
data CommonSystem = CommonSystem {
	csHG3DPath::String, -- ^ the path, where the binaries of HGamer3D are installed (install directory, without bin)
	csProgPath::String -- ^ the path, from which the final linked executable has been started
}

-- | 3D Graphics objects (Ogre specific)	
data Graphics3DSystem = Graphics3DSystem {
	g3sRoot::HG3DClass, 
	g3sSceneManager::HG3DClass,
	g3sResourceGroupManager::HG3DClass,
	g3sTextureManager::HG3DClass,
	g3sControllerManager::HG3DClass,
	g3sLogManager::HG3DClass,
	g3sCamera::HG3DClass,
	g3sRenderWindow::HG3DClass,
	g3sViewport::HG3DClass,
	g3sMessagePump::HG3DClass
} 

-- | GUI System objects (CEGUI specific)
data GUISystem = GUISystem {
	guiRenderer::HG3DClass,
	guiGUI::HG3DClass,
	guiWindowManager::HG3DClass,
	guiWindowManagerHG3D::HG3DClass,
	guiFontManager::HG3DClass,
	guiSchemeManager::HG3DClass,
	guiEventController::HG3DClass
} 

-- | Network System objects
data NetworkSystem = NetworkSystem {
	nsNetwork::HG3DClass
}

-- | Physics System objects
data PhysicsSystem = PhysicsSystem {
	psPhysics::HG3DClass,
	psWorld::HG3DClass
}

-- | Data, which can be read by all sub-system implementations, but not modified
data HG3DReaderState = HG3DReaderState {
	commonSystem::CommonSystem,
	graphics3DSystem::Graphics3DSystem,
	guiSystem::GUISystem,
	networkSystem::NetworkSystem,
	physicsSystem::PhysicsSystem
}

-- | Data, which can be read and modified (for State monad)
data HG3DEngineState = HG3DEngineState {
	esUniqueNumbers::[Integer],
        esEventMap::EventMap
}

-- | the HGamer3D monad, a monad stack of reader and state monad
type MHGamer3D a = (ReaderT HG3DReaderState) (StateT HG3DEngineState IO) a

-- | summary data type for all types of Events
data Event = 
  GUIEvent String String HG3DClass -- ^ GUI Event: name sender window

-- | an event function is reacting on an event with a side effect only
type EventFunction = Event -> MHGamer3D ()

-- | map of all events
type EventMap = Map.Map String EventFunction

-- | the time type of HGamer3D (in milliseconds)
data TimeMS = TimeMS Int			-- time in milliseconds

instance Show TimeMS where
	show (TimeMS s) = (show s) ++ " Milliseconds"

