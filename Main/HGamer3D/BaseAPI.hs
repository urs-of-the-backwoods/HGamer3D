-- This source file is part of HGamer3D
-- (A project to enable 3D game development in Haskell)
-- For the latest info, see http://www.althainz.de/HGamer3D.html
--
-- (c) 2014 Peter Althainz
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

-- HGamer3D BaseAPI Module
--

-- | HGamer3D - A game engine for the Haskell Programmer, this module includes the common modules for the basic API.
module HGamer3D.BaseAPI

(
  module HGamer3D.Data,
  module HGamer3D.Util,

  module HGamer3D.Graphics3D,
  module HGamer3D.WinEvent,
  module HGamer3D.GUI,

  HG3DEvent (..),
  
  initHGamer3D,
  freeHGamer3D,
  stepHGamer3D,
  
  ) where

  import HGamer3D.Data
  import HGamer3D.Util

  import HGamer3D.Graphics3D
  import HGamer3D.WinEvent
  import HGamer3D.GUI

  import HGamer3D.Internal.GameLoop 
