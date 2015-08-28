-- This source file is part of HGamer3D
-- (A project to enable 3D game development in Haskell)
-- For the latest info, see http://www.hgamer3d.org
--
-- (c) 2011 - 2015 Peter Althainz
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

-- HGamer3D/Binding/Helper.hs

{-# LANGUAGE ForeignFunctionInterface #-}

-- | Helper functions for binding ffi, encoding, decoding via messagepack
module HGamer3D.Binding.Helper

where

import Data.ByteString
import Data.ByteString.Unsafe
import qualified Data.ByteString.Lazy as BL
import Data.MessagePack
import Data.Either
import Data.Maybe
import Data.Serialize

import Foreign
import Foreign.C
import Foreign.Ptr

import HGamer3D.Binding.CFunctions
import HGamer3D.Data.Component

toMsg :: ComponentClass o => o -> ByteString
toMsg o = encode (toObj o)

fromMsg :: ComponentClass o => ByteString -> Maybe o
fromMsg bs = case decode bs of 
                Right o -> Just $ fromObj o
                _ -> Nothing

-- helper functions

type MsgFunction = Ptr () -> Ptr CChar -> CInt -> IO CInt
foreign import ccall "dynamic" 
   mkMsgFun :: FunPtr MsgFunction -> MsgFunction
foreign import ccall "wrapper"
   mkMsgFunPtr :: MsgFunction -> IO (FunPtr MsgFunction)

callMsgFunction :: FunPtr MsgFunction -> Ptr () -> ByteString -> IO Int
callMsgFunction mf p msg = do
      let f = mkMsgFun mf
      let dat = msg
      unsafeUseAsCStringLen' dat $ \(dat'1, dat'2) -> f p dat'1  dat'2 >>= \res -> return (fromIntegral res)
--      unsafeUseAsCStringLen' dat $ \(dat'1, dat'2) -> print "msgfun" >> print dat'1 >> print dat'2 >> f p dat'1  dat'2 >>= \res -> return (fromIntegral res)

type InitFunction = Ptr () -> IO CInt
foreign import ccall "dynamic" 
   mkInitFun :: FunPtr InitFunction -> InitFunction

callInitFunction :: FunPtr InitFunction -> Ptr () -> IO Int
callInitFunction ifp p = do
    let f = mkInitFun ifp
    res <- f p
    return (fromIntegral res)

