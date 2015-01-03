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

-- | 3D Graphics for HGamer3D, public API.
module HGamer3D.BaseAPI.Graphics3D

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

  -- * Graphics3D Base API
  module HGamer3D.Graphics3D.Graphics3DBase,

  -- * Graphics3D Schema
--  module HGamer3D.Graphics3D.Graphics3DSchema,

  -- * GUI Base API
  module HGamer3D.Graphics3D.GUIBase,

  -- * GUI Schema
--  module HGamer3D.Graphics3D.GUISchema,

  -- * WinEvent
  module HGamer3D.Graphics3D.WinEvent,

  -- * Events, GameLoop, other
  module HGamer3D.Graphics3D.Event,
  module HGamer3D.Graphics3D.GameLoop

)

where

  import HGamer3D.Graphics3D.Graphics3DBase
  -- import HGamer3D.Graphics3D.Graphics3DSchema
  import HGamer3D.Graphics3D.GUIBase
  --import HGamer3D.Graphics3D.GUISchema
  import HGamer3D.Graphics3D.WinEvent
  import HGamer3D.Graphics3D.Event
  import HGamer3D.Graphics3D.GameLoop
  
  import HGamer3D.Bindings.SDL2.EnumSDLEventType
  import HGamer3D.Bindings.SDL2.EnumSDLKeymod
  import HGamer3D.Bindings.SDL2.EnumSDLScancode
  import HGamer3D.Bindings.SDL2.EnumSDLWindowEventID
  import HGamer3D.Bindings.SDL2.EnumSDLWindowFlags
  import HGamer3D.Bindings.SDL2.StructSDLEvent

  



