{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE TypeSynonymInstances #-}

-- This source file is part of HGamer3D, a project to enable 3D game development 
-- in Haskell. For the latest info, see http://www.hgamer3d.org .
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
-- 


-- HeaderSDLEvents.chs

-- 

module HGamer3D.Bindings.SDL2.HeaderSDLEvents where

import Foreign
import Foreign.Ptr
import Foreign.C

import HGamer3D.Data.HG3DClass
import HGamer3D.Data.Vector
import HGamer3D.Data.Colour
import HGamer3D.Data.Angle

{# import HGamer3D.Bindings.SDL2.Utils #}
{# import HGamer3D.Bindings.SDL2.ClassPtr #}
{# import HGamer3D.Bindings.SDL2.StructHG3DClass #}
{# import HGamer3D.Bindings.SDL2.StructSDLEvent #}

#include "HeaderSDLEvents.h"
{- function sdl_PumpEvents -}
{#fun sdlevts_sdl_PumpEvents as sdlPumpEvents 
{ } -> `()'  #}

{- function sdl_FlushEvent -}
{#fun sdlevts_sdl_FlushEvent as sdlFlushEvent 
{ fromIntegral `Int' } -> `()'  #}

{- function sdl_FlushEvents -}
{#fun sdlevts_sdl_FlushEvents as sdlFlushEvents 
{ fromIntegral `Int' ,
 fromIntegral `Int' } -> `()'  #}

{- function sdl_PollEvent -}
{#fun sdlevts_sdl_PollEvent as sdlPollEvent 
{ alloca- `SDLEvent' peekSDLEvent*,
 alloca- `Int' peekIntConv*} -> `()'  #}

{- function sdl_WaitEvent -}
{#fun sdlevts_sdl_WaitEvent as sdlWaitEvent 
{ alloca- `SDLEvent' peekSDLEvent*,
 alloca- `Int' peekIntConv*} -> `()'  #}

{- function sdl_WaitEventTimeout -}
{#fun sdlevts_sdl_WaitEventTimeout as sdlWaitEventTimeout 
{ alloca- `SDLEvent' peekSDLEvent*,
 fromIntegral `Int' ,
 alloca- `Int' peekIntConv*} -> `()'  #}

{- function sdl_PushEvent -}
{#fun sdlevts_sdl_PushEvent as sdlPushEvent 
{ alloca- `SDLEvent' peekSDLEvent*,
 alloca- `Int' peekIntConv*} -> `()'  #}

{- function sdl_RegisterEvents -}
{#fun sdlevts_sdl_RegisterEvents as sdlRegisterEvents 
{ fromIntegral `Int' ,
 alloca- `Int' peekIntConv*} -> `()'  #}

