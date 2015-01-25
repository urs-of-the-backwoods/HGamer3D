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

module HGamer3D

(
  module HGamer3D.Data,
  module HGamer3D.Common,
  module HGamer3D.Graphics3D,
  module HGamer3D.Audio,
  module HGamer3D.Network,
  module HGamer3D.InputSystem,

  forkHGamer3D

) where

import HGamer3D.Data
import HGamer3D.Common
import HGamer3D.Graphics3D
import HGamer3D.Audio
import HGamer3D.Network
import HGamer3D.InputSystem

forkHGamer3D = do
    graphicsWorld <- forkGraphics3DWorld (msecT 30)
    audioWorld <- forkAudioWorld (msecT 100)
    networkWorld <- forkNetworkWorld (msecT 50)
    inputSystemWorld <- forkInputSystemWorld (msecT 100)
    let world = graphicsWorld #+ audioWorld #+ networkWorld #+ inputSystemWorld
    return world
