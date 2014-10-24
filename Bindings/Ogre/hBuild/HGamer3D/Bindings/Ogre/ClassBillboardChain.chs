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


-- ClassBillboardChain.chs

-- 

module HGamer3D.Bindings.Ogre.ClassBillboardChain where

import Foreign
import Foreign.Ptr
import Foreign.C

import HGamer3D.Data.HG3DClass
import HGamer3D.Data.Vector
import HGamer3D.Data.Colour
import HGamer3D.Data.Angle

{# import HGamer3D.Bindings.Ogre.Utils #}
{# import HGamer3D.Bindings.Ogre.ClassPtr #}
{# import HGamer3D.Bindings.Ogre.StructHG3DClass #}
{# import HGamer3D.Bindings.Ogre.EnumBillboardChainTexCoordDirection #}
{# import HGamer3D.Bindings.Ogre.StructVec3 #}
{# import HGamer3D.Bindings.Ogre.StructSharedPtr #}

#include "ClassBillboardChain.h"
{- function ~BillboardChain -}
{#fun ogre_bbdc_destruct as delete 
{ withHG3DClass* `HG3DClass' } -> `()'  #}

{- function setMaxChainElements -}
{#fun ogre_bbdc_setMaxChainElements as setMaxChainElements 
{ withHG3DClass* `HG3DClass' ,
 fromIntegral `Int' } -> `()'  #}

{- function getMaxChainElements -}
{#fun ogre_bbdc_getMaxChainElements as getMaxChainElements 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Int' peekIntConv*} -> `()'  #}

{- function setNumberOfChains -}
{#fun ogre_bbdc_setNumberOfChains as setNumberOfChains 
{ withHG3DClass* `HG3DClass' ,
 fromIntegral `Int' } -> `()'  #}

{- function getNumberOfChains -}
{#fun ogre_bbdc_getNumberOfChains as getNumberOfChains 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Int' peekIntConv*} -> `()'  #}

{- function setUseTextureCoords -}
{#fun ogre_bbdc_setUseTextureCoords as setUseTextureCoords 
{ withHG3DClass* `HG3DClass' ,
 fromBool `Bool' } -> `()'  #}

{- function getUseTextureCoords -}
{#fun ogre_bbdc_getUseTextureCoords as getUseTextureCoords 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function setTextureCoordDirection -}
{#fun ogre_bbdc_setTextureCoordDirection as setTextureCoordDirection 
{ withHG3DClass* `HG3DClass' ,
 cIntFromEnum `EnumBillboardChainTexCoordDirection' } -> `()'  #}

{- function getTextureCoordDirection -}
{#fun ogre_bbdc_getTextureCoordDirection as getTextureCoordDirection 
{ withHG3DClass* `HG3DClass' ,
 alloca- `EnumBillboardChainTexCoordDirection' peekEnumUtil*} -> `()'  #}

{- function setOtherTextureCoordRange -}
{#fun ogre_bbdc_setOtherTextureCoordRange as setOtherTextureCoordRange 
{ withHG3DClass* `HG3DClass' ,
 realToFrac `Float' ,
 realToFrac `Float' } -> `()'  #}

{- function setUseVertexColours -}
{#fun ogre_bbdc_setUseVertexColours as setUseVertexColours 
{ withHG3DClass* `HG3DClass' ,
 fromBool `Bool' } -> `()'  #}

{- function getUseVertexColours -}
{#fun ogre_bbdc_getUseVertexColours as getUseVertexColours 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function setDynamic -}
{#fun ogre_bbdc_setDynamic as setDynamic 
{ withHG3DClass* `HG3DClass' ,
 fromBool `Bool' } -> `()'  #}

{- function getDynamic -}
{#fun ogre_bbdc_getDynamic as getDynamic 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function removeChainElement -}
{#fun ogre_bbdc_removeChainElement as removeChainElement 
{ withHG3DClass* `HG3DClass' ,
 fromIntegral `Int' } -> `()'  #}

{- function getNumChainElements -}
{#fun ogre_bbdc_getNumChainElements as getNumChainElements 
{ withHG3DClass* `HG3DClass' ,
 fromIntegral `Int' ,
 alloca- `Int' peekIntConv*} -> `()'  #}

{- function clearChain -}
{#fun ogre_bbdc_clearChain as clearChain 
{ withHG3DClass* `HG3DClass' ,
 fromIntegral `Int' } -> `()'  #}

{- function clearAllChains -}
{#fun ogre_bbdc_clearAllChains as clearAllChains 
{ withHG3DClass* `HG3DClass' } -> `()'  #}

{- function setFaceCamera -}
{#fun ogre_bbdc_setFaceCamera as setFaceCamera 
{ withHG3DClass* `HG3DClass' ,
 fromBool `Bool' ,
 withVec3* `Vec3' } -> `()'  #}

{- function getMaterialName -}
{#fun ogre_bbdc_getMaterialName as getMaterialName 
{ withHG3DClass* `HG3DClass' ,
 alloc64k- `String' peekCString*} -> `()'  #}

{- function setMaterialName -}
{#fun ogre_bbdc_setMaterialName as setMaterialName 
{ withHG3DClass* `HG3DClass' ,
 withCString* `String' ,
 withCString* `String' } -> `()'  #}

{- function getSquaredViewDepth -}
{#fun ogre_bbdc_getSquaredViewDepth as getSquaredViewDepth 
{ withHG3DClass* `HG3DClass' ,
 withHG3DClass* `HG3DClass' ,
 alloca- `Float' peekFloatConv*} -> `()'  #}

{- function getBoundingRadius -}
{#fun ogre_bbdc_getBoundingRadius as getBoundingRadius 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Float' peekFloatConv*} -> `()'  #}

{- function getMaterial -}
{#fun ogre_bbdc_getMaterial as getMaterial 
{ withHG3DClass* `HG3DClass' ,
 alloca- `SharedPtr' peekSharedPtr*} -> `()'  #}

{- function getMovableType -}
{#fun ogre_bbdc_getMovableType as getMovableType 
{ withHG3DClass* `HG3DClass' ,
 alloc64k- `String' peekCString*} -> `()'  #}

{- function preRender -}
{#fun ogre_bbdc_preRender as preRender 
{ withHG3DClass* `HG3DClass' ,
 withHG3DClass* `HG3DClass' ,
 withHG3DClass* `HG3DClass' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

