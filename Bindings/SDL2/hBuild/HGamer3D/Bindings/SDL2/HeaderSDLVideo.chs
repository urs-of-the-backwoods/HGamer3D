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


-- HeaderSDLVideo.chs

-- 

module HGamer3D.Bindings.SDL2.HeaderSDLVideo where

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

#include "HeaderSDLVideo.h"
{- function sdl_GetNumVideoDrivers -}
{#fun fsdlvid_sdl_GetNumVideoDrivers as sdlGetNumVideoDrivers 
{ alloca- `Int' peekIntConv*} -> `()'  #}

{- function sdl_GetVideoDriver -}
{#fun fsdlvid_sdl_GetVideoDriver as sdlGetVideoDriver 
{ fromIntegral `Int' ,
 alloc64k- `String' peekCString*} -> `()'  #}

{- function sdl_VideoInit -}
{#fun fsdlvid_sdl_VideoInit as sdlVideoInit 
{ withCString* `String' ,
 alloca- `Int' peekIntConv*} -> `()'  #}

{- function sdl_VideoQuit -}
{#fun fsdlvid_sdl_VideoQuit as sdlVideoQuit 
{ } -> `()'  #}

{- function sdl_GetCurrentVideoDriver -}
{#fun fsdlvid_sdl_GetCurrentVideoDriver as sdlGetCurrentVideoDriver 
{ alloc64k- `String' peekCString*} -> `()'  #}

{- function sdl_GetNumVideoDisplays -}
{#fun fsdlvid_sdl_GetNumVideoDisplays as sdlGetNumVideoDisplays 
{ alloca- `Int' peekIntConv*} -> `()'  #}

{- function sdl_GetDisplayName -}
{#fun fsdlvid_sdl_GetDisplayName as sdlGetDisplayName 
{ fromIntegral `Int' ,
 alloc64k- `String' peekCString*} -> `()'  #}

{- function sdl_GetNumDisplayModes -}
{#fun fsdlvid_sdl_GetNumDisplayModes as sdlGetNumDisplayModes 
{ fromIntegral `Int' ,
 alloca- `Int' peekIntConv*} -> `()'  #}

{- function sdl_GetWindowDisplayIndex -}
{#fun fsdlvid_sdl_GetWindowDisplayIndex as sdlGetWindowDisplayIndex 
{ castPtr `Ptr SDLWindow' ,
 alloca- `Int' peekIntConv*} -> `()'  #}

{- function sdl_GetWindowPixelFormat -}
{#fun fsdlvid_sdl_GetWindowPixelFormat as sdlGetWindowPixelFormat 
{ castPtr `Ptr SDLWindow' ,
 alloca- `Int' peekIntConv*} -> `()'  #}

{- function sdl_CreateWindow -}
{#fun fsdlvid_sdl_CreateWindow as sdlCreateWindow 
{ withCString* `String' ,
 fromIntegral `Int' ,
 fromIntegral `Int' ,
 fromIntegral `Int' ,
 fromIntegral `Int' ,
 fromIntegral `Int' ,
 alloca- `Ptr SDLWindow' peekSDLWindowPtr*} -> `()'  #}

{- function sdl_GetWindowID -}
{#fun fsdlvid_sdl_GetWindowID as sdlGetWindowID 
{ castPtr `Ptr SDLWindow' ,
 alloca- `Int' peekIntConv*} -> `()'  #}

{- function sdl_GetWindowFromID -}
{#fun fsdlvid_sdl_GetWindowFromID as sdlGetWindowFromID 
{ fromIntegral `Int' ,
 alloca- `Ptr SDLWindow' peekSDLWindowPtr*} -> `()'  #}

{- function sdl_GetWindowFlags -}
{#fun fsdlvid_sdl_GetWindowFlags as sdlGetWindowFlags 
{ castPtr `Ptr SDLWindow' ,
 alloca- `Int' peekIntConv*} -> `()'  #}

{- function sdl_SetWindowTitle -}
{#fun fsdlvid_sdl_SetWindowTitle as sdlSetWindowTitle 
{ castPtr `Ptr SDLWindow' ,
 withCString* `String' } -> `()'  #}

{- function sdl_GetWindowTitle -}
{#fun fsdlvid_sdl_GetWindowTitle as sdlGetWindowTitle 
{ castPtr `Ptr SDLWindow' ,
 alloc64k- `String' peekCString*} -> `()'  #}

{- function sdl_SetWindowPosition -}
{#fun fsdlvid_sdl_SetWindowPosition as sdlSetWindowPosition 
{ castPtr `Ptr SDLWindow' ,
 fromIntegral `Int' ,
 fromIntegral `Int' } -> `()'  #}

{- function sdl_GetWindowPosition -}
{#fun fsdlvid_sdl_GetWindowPosition as sdlGetWindowPosition 
{ castPtr `Ptr SDLWindow' ,
 alloca- `Int' peekIntConv*,
 alloca- `Int' peekIntConv*} -> `()'  #}

{- function sdl_SetWindowSize -}
{#fun fsdlvid_sdl_SetWindowSize as sdlSetWindowSize 
{ castPtr `Ptr SDLWindow' ,
 fromIntegral `Int' ,
 fromIntegral `Int' } -> `()'  #}

{- function sdl_GetWindowSize -}
{#fun fsdlvid_sdl_GetWindowSize as sdlGetWindowSize 
{ castPtr `Ptr SDLWindow' ,
 alloca- `Int' peekIntConv*,
 alloca- `Int' peekIntConv*} -> `()'  #}

{- function sdl_SetWindowMinimumSize -}
{#fun fsdlvid_sdl_SetWindowMinimumSize as sdlSetWindowMinimumSize 
{ castPtr `Ptr SDLWindow' ,
 fromIntegral `Int' ,
 fromIntegral `Int' } -> `()'  #}

{- function sdl_GetWindowMinimumSize -}
{#fun fsdlvid_sdl_GetWindowMinimumSize as sdlGetWindowMinimumSize 
{ castPtr `Ptr SDLWindow' ,
 alloca- `Int' peekIntConv*,
 alloca- `Int' peekIntConv*} -> `()'  #}

{- function sdl_SetWindowMaximumSize -}
{#fun fsdlvid_sdl_SetWindowMaximumSize as sdlSetWindowMaximumSize 
{ castPtr `Ptr SDLWindow' ,
 fromIntegral `Int' ,
 fromIntegral `Int' } -> `()'  #}

{- function sdl_GetWindowMaximumSize -}
{#fun fsdlvid_sdl_GetWindowMaximumSize as sdlGetWindowMaximumSize 
{ castPtr `Ptr SDLWindow' ,
 alloca- `Int' peekIntConv*,
 alloca- `Int' peekIntConv*} -> `()'  #}

{- function sdl_ShowWindow -}
{#fun fsdlvid_sdl_ShowWindow as sdlShowWindow 
{ castPtr `Ptr SDLWindow' } -> `()'  #}

{- function sdl_HideWindow -}
{#fun fsdlvid_sdl_HideWindow as sdlHideWindow 
{ castPtr `Ptr SDLWindow' } -> `()'  #}

{- function sdl_RaiseWindow -}
{#fun fsdlvid_sdl_RaiseWindow as sdlRaiseWindow 
{ castPtr `Ptr SDLWindow' } -> `()'  #}

{- function sdl_MaximizeWindow -}
{#fun fsdlvid_sdl_MaximizeWindow as sdlMaximizeWindow 
{ castPtr `Ptr SDLWindow' } -> `()'  #}

{- function sdl_MinimizeWindow -}
{#fun fsdlvid_sdl_MinimizeWindow as sdlMinimizeWindow 
{ castPtr `Ptr SDLWindow' } -> `()'  #}

{- function sdl_RestoreWindow -}
{#fun fsdlvid_sdl_RestoreWindow as sdlRestoreWindow 
{ castPtr `Ptr SDLWindow' } -> `()'  #}

{- function sdl_SetWindowFullscreen -}
{#fun fsdlvid_sdl_SetWindowFullscreen as sdlSetWindowFullscreen 
{ castPtr `Ptr SDLWindow' ,
 fromIntegral `Int' ,
 alloca- `Int' peekIntConv*} -> `()'  #}

{- function sdl_UpdateWindowSurface -}
{#fun fsdlvid_sdl_UpdateWindowSurface as sdlUpdateWindowSurface 
{ castPtr `Ptr SDLWindow' ,
 alloca- `Int' peekIntConv*} -> `()'  #}

{- function sdl_SetWindowBrightness -}
{#fun fsdlvid_sdl_SetWindowBrightness as sdlSetWindowBrightness 
{ castPtr `Ptr SDLWindow' ,
 realToFrac `Float' ,
 alloca- `Int' peekIntConv*} -> `()'  #}

{- function sdl_GetWindowBrightness -}
{#fun fsdlvid_sdl_GetWindowBrightness as sdlGetWindowBrightness 
{ castPtr `Ptr SDLWindow' ,
 alloca- `Float' peekFloatConv*} -> `()'  #}

{- function sdl_DestroyWindow -}
{#fun fsdlvid_sdl_DestroyWindow as sdlDestroyWindow 
{ castPtr `Ptr SDLWindow' } -> `()'  #}

{- function sdl_EnableScreenSaver -}
{#fun fsdlvid_sdl_EnableScreenSaver as sdlEnableScreenSaver 
{ } -> `()'  #}

{- function sdl_DisableScreenSaver -}
{#fun fsdlvid_sdl_DisableScreenSaver as sdlDisableScreenSaver 
{ } -> `()'  #}

