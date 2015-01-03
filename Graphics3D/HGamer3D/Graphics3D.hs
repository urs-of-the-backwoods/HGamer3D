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

-- Graphics.hs

-- | 3D Graphics for HGamer3D, public API.

module HGamer3D.Graphics3D

(
  module HGamer3D.Graphics3D.Graphics3DSchema
  , module HGamer3D.Graphics3D.GUISchema
  , module HGamer3D.Graphics3D.Event

  , forkGraphics3DWorld,
    regQuitHandler
)

where

  import HGamer3D.Graphics3D.Graphics3DSchema
  import HGamer3D.Graphics3D.GUISchema
  import HGamer3D.Graphics3D.Event
  import HGamer3D.Graphics3D.SystemGraphics3D


