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

{#fun entity_create as entityCreate
	    { 
			unsafeUseAsCStringLen'* `ByteString'& , 
	        alloca- `Ptr ()' peek*
        } -> `()'
#}

{#fun entity_set as entitySet
	    { 
			unsafeUseAsCStringLen'* `ByteString'& , 
	        `Ptr ()'
        } -> `()'
#}

{#fun callback_system_create as callbackSystemCreate
	    { 
	        alloca- `Ptr ()' peek*
        } -> `()'
#}

{#fun callback_system_register_receiver as callbackSystemRegisterReceiver
	    { 
	        id `Ptr ()',
	        id `Ptr ()',
 			`Word64',
 			id `FunPtr (Ptr () -> Ptr CChar -> CInt -> IO CInt)'
        } -> `()'
#}

{#fun callback_system_step as callbackSystemStep
	    { 
	        id `Ptr ()'
        } -> `()'
#}
