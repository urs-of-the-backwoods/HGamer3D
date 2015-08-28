-- This source file is part of HGamer3D
-- (A project to enable 3D game development in Haskell)
-- For the latest info, see http://www.hgamer3d.org
-- 
-- (c) 2015 Peter Althainz
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
-- HGamer3D/Binding/CFunctions.hs
-- 

{-# LANGUAGE ForeignFunctionInterface #-}

#include "interface.h"

-- | C-function interface towards the urho3d dll
module HGamer3D.Binding.CFunctions where

import Foreign
import Foreign.C
import Data.Word
import Data.ByteString
import Data.ByteString.Unsafe

type CStringCLen i = (CString, i)

unsafeUseAsCStringLen' :: (Integral i) => ByteString -> (CStringCLen i -> IO a) -> IO a
unsafeUseAsCStringLen' str fn =
  unsafeUseAsCStringLen str (\(ptr, len) -> fn (ptr, fromIntegral len))

{#fun hg3durho3d0_create_item as createItem
	       { 
	       `Word64',
	       unsafeUseAsCStringLen'* `ByteString'& , 
	       alloca- `Ptr ()' peek*
               } -> `Int' fromIntegral
	     #}

{#fun hg3durho3d0_destroy_item as destroyItem
	        {
	       `Word64',
		   id `Ptr ()' 
		} -> `Int' fromIntegral
	      #}

{#fun hg3durho3d0_get_msg_sender as getMessageSender
	    {
	       `Word64',
	       `Word64',
	        alloca- `FunPtr (Ptr () -> Ptr CChar -> CInt -> IO CInt)' peek*
	    } -> `Int' fromIntegral
	 #}

{#fun hg3durho3d0_register_msg_receiver as registerMessageReceiver
	    {
	       `Word64',
	       `Word64',
		   id `Ptr ()',
	       id `FunPtr (Ptr () -> Ptr CChar -> CInt -> IO CInt)'
	    } -> `Int' fromIntegral
	 #}

{#fun hg3durho3d0_error_message as errorMessage
	         {
		        `Int'
		     } -> `String' peekCString*
	 #}
		     

