{-# OPTIONS_HADDOCK hide #-}

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

-- HGamer3D/Internal/Event Module
--

module HGamer3D.Engine.Internal.Event
where

import HGamer3D.WinEvent.BaseAPI
import HGamer3D.GUI.BaseAPI
import qualified HGamer3D.GUI.Schema.Widget as W
import qualified HGamer3D.GUI.Schema.Form as F
import Data.Dynamic

data AudioEvent = PlaySound String
                | StopSound String
                  deriving (Show)

data GUIFormEvent = FormSetValue [(String, F.FormValue)]
                  | FormValueChange String [(String, F.FormValue)]
                  | FormButtonClick String
                    deriving (Show)

data HG3DEvent = WindowEvt SDLEvent
               | GUIEvt GUIEvent                         -- ^ Low level GUI evts
               | FormEvt GUIFormEvent                    -- ^ High level GUI evts, directly form related
               | AudioEvt AudioEvent
               | UserEvt Dynamic
                 deriving (Show)
                 

  
