{-# Language DeriveDataTypeable #-}
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

module HGamer3D.Graphics3D.Event
where

import HGamer3D.Graphics3D.WinEvent
import HGamer3D.Graphics3D.GUIBase
import HGamer3D.Graphics3D.GUISchema
import Data.Dynamic

data GUIFormEvent = FormSetValue [(String, FormValue)]
                  | FormValueChange String [(String, FormValue)]
                  | FormButtonClick String
                    deriving (Typeable, Show)

data ApplicationEvent = AppQuit
     		      deriving (Typeable, Show)

data HG3DEvent = WindowEvt SDLEvent
               | GUIEvt GUIEvent                         -- ^ Low level GUI evts
               | FormEvt GUIFormEvent                    -- ^ High level GUI evts, directly form related
	       | AppEvt ApplicationEvent
                 deriving (Typeable, Show)
                 
data EventType = WinEvents | GUIEvents | FormEvents | ApplicationEvents | AllEvents deriving (Typeable, Show)

filterEventType :: [EventType] -> [HG3DEvent] -> [HG3DEvent]
filterEventType types events = let
  isTypeOf evt evttype = case evttype of
    WinEvents -> case evt of
      (WindowEvt _) -> True
      _ -> False
    GUIEvents -> case evt of
      (GUIEvt _) -> True
      _ -> False
    FormEvents -> case evt of
      (FormEvt _) -> True
      _ -> False
    ApplicationEvents -> case evt of
      (AppEvt _) -> True
      _ -> False
    AllEvents -> True
    _ -> False
  in concat (map (\t -> filter (\e -> isTypeOf e t) events) types)
