-- This source file is part of HGamer3D
-- (A project to enable 3D game development in Haskell)
-- For the latest info, see http://www.hgamer3d.org
--
-- (c) 2011-2013 Peter Althainz
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

-- | 3D Graphics for HGamer3D
module HGamer3D.Graphics3D

(
  module HGamer3D.Data,
  module HGamer3D.Util,
  module HGamer3D.Graphics3D.Types,
  module HGamer3D.Graphics3D.Basic3D,
  module HGamer3D.Graphics3D.Object3D,
  module HGamer3D.Graphics3D.Light,
  module HGamer3D.Graphics3D.PlatonObjects,
  module HGamer3D.Graphics3D.Engine,
  initHGamer3D,
  exitHGamer3D,
  loopHGamer3D
)

where

  import HGamer3D.Data
  import HGamer3D.Util
  import HGamer3D.Graphics3D.Types
  import HGamer3D.Graphics3D.Basic3D
  import HGamer3D.Graphics3D.Object3D
  import HGamer3D.Graphics3D.Light
  import HGamer3D.Graphics3D.PlatonObjects
  import HGamer3D.Graphics3D.Engine

  initHGamer3D windowName sceneManagerType fConfig fLog = do
    (g3ds, camera, viewport, window) <- initGraphics3D windowName sceneManagerType fConfig fLog
    return (g3ds, camera, viewport)
    
  exitHGamer3D = exitGraphics3D
  
  loopHGamer3D = loopGraphics3D



