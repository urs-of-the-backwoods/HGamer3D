{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE TypeSynonymInstances #-}

-- This source file is part of HGamer3D
-- (A project to enable 3D game development in Haskell)
-- For the latest info, see http://www.althainz.de/HGamer3D.html
-- 
-- (c) 2011 Peter Althainz
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
-- HG3DClass.hs
-- 

module HGamer3D.Data.HG3DClass where

-- import C2HS
import Foreign
import Foreign.Ptr
import Foreign.C


import Data.Bits

data HG3DClass = HG3DClass {
  ocPtr :: Ptr (),
  ocFptr :: Ptr ()
   } deriving (Eq, Show)

instance Storable HG3DClass where
  alignment _ = alignment (undefined :: CDouble)
  sizeOf _ = 8
  
  peek p = do
	ptr <- (\ptr -> do {peekByteOff ptr 0 ::IO (Ptr ())}) p
	fptr <- (\ptr -> do {peekByteOff ptr 4 ::IO (Ptr ())}) p
	let hc = HG3DClass (ptr) (fptr)
	return hc
	
  poke p (HG3DClass ptr fptr) = do
    (\ptr val -> do {pokeByteOff ptr 0 (val::(Ptr ()))}) p (ptr)
    (\ptr val -> do {pokeByteOff ptr 4 (val::(Ptr ()))}) p (fptr)
    



 
