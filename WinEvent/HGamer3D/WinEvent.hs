-- This source file is part of HGamer3D
-- (A project to enable 3D game development in Haskell)
-- For the latest info, see http://www.hgamer3d.org
--
-- (c) 2011-2014 Peter Althainz
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

-- WinEvents.hs


-- | Windowing and Event functionality for HGamer3D, public API. This module abstracts the functionality needed to create a window and to handle events from this window over different platforms. This module currently is not useful standalone, but is used in HGamer3D (main module) to construct initialization and game loop functionality.
module HGamer3D.WinEvent
(
  -- * Basic Data Types and Enums
  EnumWinEventInit (..),
  SDLSystem,
  module HGamer3D.Bindings.SDL2.StructSDLEvent,
  module HGamer3D.Bindings.SDL2.EnumSDLEventType,
  module HGamer3D.Bindings.SDL2.EnumSDLKeymod,
  module HGamer3D.Bindings.SDL2.EnumSDLScancode,
  module HGamer3D.Bindings.SDL2.EnumSDLWindowEventID,
  module HGamer3D.Bindings.SDL2.EnumSDLWindowFlags,

  -- * Initialization and Game Loop Functions
  initWinEvent,
  freeWinEvent,
  pollWinEvent,

  -- * Misc Functions
  openWindow,
)

where

import HGamer3D.WinEvent.Internal.Base

import HGamer3D.Bindings.SDL2.EnumSDLEventType
import HGamer3D.Bindings.SDL2.EnumSDLKeymod
import HGamer3D.Bindings.SDL2.EnumSDLScancode
import HGamer3D.Bindings.SDL2.EnumSDLWindowEventID
import HGamer3D.Bindings.SDL2.EnumSDLWindowFlags
import HGamer3D.Bindings.SDL2.StructSDLEvent
