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


-- StructSDLEvent.chs

-- 

module HGamer3D.Bindings.SDL2.StructSDLEvent where

import Foreign
import Foreign.Ptr
import Foreign.C

import HGamer3D.Data.HG3DClass
import HGamer3D.Data.Vector
import HGamer3D.Data.Colour
import HGamer3D.Data.Angle


#include "StructSDLEvent.h"

import HGamer3D.Data
import HGamer3D.Data.Window

import HGamer3D.Bindings.SDL2.EnumSDLEventType
import HGamer3D.Bindings.SDL2.EnumSDLKeymod
import HGamer3D.Bindings.SDL2.EnumSDLScancode
import HGamer3D.Bindings.SDL2.EnumSDLWindowEventID

import Control.Monad
import Control.Applicative
import Control.Exception

import qualified Data.ByteString.Char8 as BSC8
import qualified Data.String.UTF8 as UTF8

import Foreign.Storable
import Foreign.C.Types
import Foreign.Ptr

type Keycode = Word32
type Keymod = Word16

type MouseID = Int

data SDLButton = SDLButtonLeft | SDLButtonRight | SDLButtonMiddle | SDLButtonX1 | SDLButtonX2 | SDLButtonNumber Int deriving (Eq, Show)

_getButton :: Word8 -> SDLButton
_getButton w = case w of
   1 -> SDLButtonLeft
   2 -> SDLButtonMiddle
   3 -> SDLButtonRight
   4 -> SDLButtonX1
   5 -> SDLButtonX2
   _ -> SDLButtonNumber ((fromIntegral . toInteger) w)

data SDLEvent = EvtKeyUp TimeMS Window EnumSDLScancode Keycode Keymod |
             EvtKeyDown TimeMS Window EnumSDLScancode Keycode Keymod |
             EvtText TimeMS Window String | 
             EvtQuit TimeMS |
             EvtWindow TimeMS Window EnumSDLWindowEventID Int Int |
             EvtMouseButtonDown TimeMS Window MouseID SDLButton Int Int |
             EvtMouseButtonUp TimeMS Window MouseID SDLButton Int Int |
             EvtMouseMotion TimeMS Window MouseID Int Int Int Int | 
             EvtCommon TimeMS EnumSDLEventType |
             EvtNotValid

instance Storable SDLEvent where
  sizeOf _ = 56
  alignment _ = 4
  
  peek p = do
   typ  <- (peekByteOff p 0 :: IO CUInt)
   time <- (peekByteOff p 4 :: IO CUInt)
   Control.Exception.catch (do
      let evttyp = (toEnum . fromIntegral . toInteger) typ  -- this first throws exception if enum not ok !
      let t = TimeMS ((fromIntegral . toInteger) time)

      case (evttyp) of
         SDL_QUIT -> return $ EvtQuit t
         SDL_KEYDOWN -> EvtKeyDown <$> return t
                            <*> fmap (Window . fromIntegral . toInteger) (peekByteOff p 8 :: IO CUInt)
                            <*> fmap (toEnum . fromIntegral . toInteger) (peekByteOff p 16 :: IO CUInt)
                            <*> fmap (toEnum . fromIntegral . toInteger) (peekByteOff p 20 :: IO Keycode)
                            <*> fmap (toEnum . fromIntegral . toInteger) (peekByteOff p 24 :: IO Keymod)
         SDL_KEYUP -> EvtKeyUp <$> return t
                            <*> fmap (Window . fromIntegral . toInteger) (peekByteOff p 8 :: IO CUInt)
                            <*> fmap (toEnum . fromIntegral . toInteger) (peekByteOff p 16 :: IO CUInt)
                            <*> fmap (toEnum . fromIntegral . toInteger) (peekByteOff p 20 :: IO Keycode)
                            <*> fmap (toEnum . fromIntegral . toInteger) (peekByteOff p 24 :: IO Keymod)
         SDL_WINDOWEVENT -> EvtWindow <$> return t
                            <*> fmap (Window . fromIntegral . toInteger) (peekByteOff p 8 :: IO CUInt)
                            <*> fmap (toEnum . fromIntegral . toInteger) (peekByteOff p 12 :: IO Word8)
                            <*> fmap (fromIntegral . toInteger) (peekByteOff p 16 :: IO CInt)
                            <*> fmap (fromIntegral . toInteger) (peekByteOff p 20 :: IO CInt)
         SDL_TEXTINPUT -> EvtText <$> return t
                            <*> fmap (Window . fromIntegral . toInteger) (peekByteOff p 8 :: IO CUInt)
			    -- this text conversion is neccessary, the C-String of the event contains a UTF8-encoding
			    -- with BSC8.pack the string is converted to ByteString without additional UTF8-encoding
			    -- the remaining decoding, creates a normal (Unicode) Haskell string from that
                            <*> fmap (UTF8.toString . UTF8.fromRep . BSC8.pack) (peekCAString (castPtr (plusPtr p 12)) )
         SDL_MOUSEBUTTONDOWN -> EvtMouseButtonDown <$> return t
                            <*> fmap (Window . fromIntegral . toInteger) (peekByteOff p 8 :: IO CUInt)
                            <*> fmap (fromIntegral . toInteger) (peekByteOff p 12 :: IO CUInt )
                            <*> fmap _getButton (peekByteOff p 16 :: IO Word8)
                            <*> fmap (fromIntegral . toInteger) (peekByteOff p 20 :: IO CInt)
                            <*> fmap (fromIntegral . toInteger) (peekByteOff p 24 :: IO CInt)
         SDL_MOUSEBUTTONUP -> EvtMouseButtonUp <$> return t
                            <*> fmap (Window . fromIntegral . toInteger) (peekByteOff p 8 :: IO CUInt)
                            <*> fmap (fromIntegral . toInteger) (peekByteOff p 12 :: IO CUInt )
                            <*> fmap _getButton (peekByteOff p 16 :: IO Word8)
                            <*> fmap (fromIntegral . toInteger) (peekByteOff p 20 :: IO CInt)
                            <*> fmap (fromIntegral . toInteger) (peekByteOff p 24 :: IO CInt)
         SDL_MOUSEMOTION -> EvtMouseMotion <$> return t
                            <*> fmap (Window . fromIntegral . toInteger) (peekByteOff p 8 :: IO CUInt)
                            <*> fmap (fromIntegral . toInteger) (peekByteOff p 12 :: IO CUInt )
                            <*> fmap (fromIntegral . toInteger) (peekByteOff p 20 :: IO CInt)
                            <*> fmap (fromIntegral . toInteger) (peekByteOff p 24 :: IO CInt)
                            <*> fmap (fromIntegral . toInteger) (peekByteOff p 28 :: IO CInt)
                            <*> fmap (fromIntegral . toInteger) (peekByteOff p 32 :: IO CInt)
         _ -> return $ EvtCommon t evttyp
               ) ((\e -> return EvtNotValid) ::(SomeException -> IO SDLEvent))

  poke p evt = error "poke of SDLEvent not implemented"
    
{#pointer *sdlevent_struct as SDLEventPtr -> SDLEvent #}

withSDLEvent :: SDLEvent -> (SDLEventPtr -> IO b) -> IO b
withSDLEvent = with

peekSDLEvent :: SDLEventPtr -> IO SDLEvent
peekSDLEvent = peek

