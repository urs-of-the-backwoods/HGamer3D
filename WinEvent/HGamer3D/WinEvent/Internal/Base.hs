{-# OPTIONS_HADDOCK hide #-}
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

-- WinEvent/Internal/Base.hs


-- | Windowing and Event functionality for HGamer3D, implementation module. This modules exposes internal data structures, public API is in HGamer3D.WinEvent.
module HGamer3D.WinEvent.Internal.Base
(
  EnumWinEventInit (..),

  SDLSystem,
  initWinEvent,
  freeWinEvent,

  attachToWindow,
  openWindow,
  pollWinEvent,
  showCursor,

  module HGamer3D.Bindings.SDL2.StructSDLEvent,
  module HGamer3D.Bindings.SDL2.EnumSDLEventType,
  module HGamer3D.Bindings.SDL2.EnumSDLKeymod,
  module HGamer3D.Bindings.SDL2.EnumSDLScancode,
  module HGamer3D.Bindings.SDL2.EnumSDLWindowEventID,
  module HGamer3D.Bindings.SDL2.EnumSDLWindowFlags,
)

where

import GHC.Ptr
import HGamer3D.Data
import HGamer3D.Data.Window

import HGamer3D.Bindings.SDL2.ClassPtr
import HGamer3D.Bindings.SDL2.Utils


import HGamer3D.Bindings.SDL2.EnumSDLEventType
import HGamer3D.Bindings.SDL2.EnumSDLKeymod
import HGamer3D.Bindings.SDL2.EnumSDLScancode
import HGamer3D.Bindings.SDL2.EnumSDLWindowEventID
import HGamer3D.Bindings.SDL2.EnumSDLWindowFlags

import HGamer3D.Bindings.SDL2.StructSDLEvent

import HGamer3D.Bindings.SDL2.HeaderSDL
import HGamer3D.Bindings.SDL2.HeaderSDLVideo
import HGamer3D.Bindings.SDL2.HeaderSDLEvents
import HGamer3D.Bindings.SDL2.EnumSDLWindowFlags
import HGamer3D.Bindings.SDL2.HeaderSDLMouse
import HGamer3D.Bindings.SDL2.ClassHG3DUtilities

import Foreign.Marshal.Utils
import Control.Monad
import Control.Applicative

data SDLSystem = SDLSystem (Ptr SDLWindow)

-- WinEvent Initialization Flags
data EnumWinEventInit = WEV_INIT_VIDEO    -- ^ initialize Video subsystem
                        | WEV_INIT_TIMER  -- ^ initialize Timer subsystem
                        | WEV_INIT_EVENTS -- ^ initialize Event subsystem

instance Enum EnumWinEventInit where
 toEnum 0x20 = WEV_INIT_VIDEO
 toEnum 0x4000 = WEV_INIT_EVENTS
 toEnum 0x01 = WEV_INIT_TIMER

 fromEnum WEV_INIT_VIDEO = 0x20
 fromEnum WEV_INIT_EVENTS = 0x4000
 fromEnum WEV_INIT_TIMER = 0x01

-- show instance for SDLEvent
instance Show SDLEvent where
  show (EvtQuit ts) = "Event-Quit"
  show (EvtKeyUp ts window keyscan keycode keymode) = "Event-KeyUp " ++ (show (fromEnum keyscan))
  show (EvtKeyDown ts window keyscan keycode keymode) = "Event-KeyDown " ++ (show (fromEnum keyscan))
  show (EvtText ts window text) = "Event-Text " ++ text
  show (EvtMouseButtonUp ts window mid button x y) = "Event-MouseButtonUp  " ++ (show button) ++ " " ++ (show x) ++ " " ++ (show y) 
  show (EvtMouseButtonDown ts window mid button x y) = "Event-MouseButtonDown  " ++ (show button) ++ " " ++ (show x) ++ " " ++ (show y) 
  show (EvtMouseMotion ts window mid x y rx ry) = "Event-MouseMotion " ++ (show x) ++ " " ++ (show y) ++ " r: " ++ (show rx) ++ " " ++ (show ry)
  show (EvtWindow ts window weid x y) = "Event-Window " ++ (show (fromEnum weid)) ++ " " ++ (show x) ++ " " ++ (show y)
  show _ = "Specific Event is not in show instance"

-- | Initialization of WinEvent
initWinEvent :: [EnumWinEventInit] -- ^ Initialization Flags
                -> IO Int          -- ^ result != 0 in case of errors
initWinEvent flags = do
  res <- sdlInit $ sum (fmap fromEnum flags)
  return res
  
-- | Un-initialize WinEvent and free resources
freeWinEvent :: IO ()
freeWinEvent = sdlQuit


attachToWindow :: Window -> IO (SDLSystem)
attachToWindow (Window handle) = do
  res <- createWindowFromHandle handle
  return $ SDLSystem res

-- | open new window
openWindow :: String   -- ^ Window Display Name
              -> Int   -- ^ X coordinate of Window
              -> Int   -- ^ Y coordinate of Window
              -> Int   -- ^ Width of Window
              -> Int   -- ^ Height of Window
              -> [EnumSDLWindowFlags]  -- ^ initialization flags
              -> IO (SDLSystem)
openWindow title x y w h flags = do
  win <- sdlCreateWindow title x y w h (sum $ fmap fromEnum flags)
  return $ SDLSystem win

-- | query for events from windowing system
pollWinEvent :: IO (Maybe SDLEvent)
pollWinEvent = do
    (sdlevt, res) <- sdlPollEvent
    let r = case res of
          0 -> Nothing
          1 -> Just sdlevt
          _ -> Nothing
    return r

-- | show / hide cursor, returns previous state
showCursor :: Bool -> IO Bool
showCursor flag = do
  ival <- sdlShowCursor (fromBool flag)
  return (toBool ival)
  
