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


-- ClassBillboardSet.chs

-- 

module HGamer3D.Bindings.Ogre.ClassBillboardSet where

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
{# import HGamer3D.Bindings.Ogre.StructVec3 #}
{# import HGamer3D.Bindings.Ogre.StructColour #}
{# import HGamer3D.Bindings.Ogre.EnumBillboardOrigin #}
{# import HGamer3D.Bindings.Ogre.EnumBillboardRotationType #}
{# import HGamer3D.Bindings.Ogre.StructSharedPtr #}
{# import HGamer3D.Bindings.Ogre.EnumBillboardType #}

#include "ClassBillboardSet.h"
{- function BillboardSet2 -}
{#fun ogre_bbs_construct as new 
{ withCString* `String' ,
 fromIntegral `Int' ,
 fromBool `Bool' ,
 alloca- `HG3DClass' peek*} -> `()'  #}

{- function ~BillboardSet -}
{#fun ogre_bbs_destruct as delete 
{ withHG3DClass* `HG3DClass' } -> `()'  #}

{- function createBillboard -}
{#fun ogre_bbs_createBillboard as createBillboard 
{ withHG3DClass* `HG3DClass' ,
 withVec3* `Vec3' ,
 withColour* `Colour' ,
 alloca- `HG3DClass' peek*} -> `()'  #}

{- function createBillboard2 -}
{#fun ogre_bbs_createBillboard2 as createBillboard2 
{ withHG3DClass* `HG3DClass' ,
 realToFrac `Float' ,
 realToFrac `Float' ,
 realToFrac `Float' ,
 withColour* `Colour' ,
 alloca- `HG3DClass' peek*} -> `()'  #}

{- function getNumBillboards -}
{#fun ogre_bbs_getNumBillboards as getNumBillboards 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Int' peekIntConv*} -> `()'  #}

{- function setAutoextend -}
{#fun ogre_bbs_setAutoextend as setAutoextend 
{ withHG3DClass* `HG3DClass' ,
 fromBool `Bool' } -> `()'  #}

{- function getAutoextend -}
{#fun ogre_bbs_getAutoextend as getAutoextend 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function setSortingEnabled -}
{#fun ogre_bbs_setSortingEnabled as setSortingEnabled 
{ withHG3DClass* `HG3DClass' ,
 fromBool `Bool' } -> `()'  #}

{- function getSortingEnabled -}
{#fun ogre_bbs_getSortingEnabled as getSortingEnabled 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function setPoolSize -}
{#fun ogre_bbs_setPoolSize as setPoolSize 
{ withHG3DClass* `HG3DClass' ,
 fromIntegral `Int' } -> `()'  #}

{- function getPoolSize -}
{#fun ogre_bbs_getPoolSize as getPoolSize 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Int' peekIntConv*} -> `()'  #}

{- function clear -}
{#fun ogre_bbs_clear as clear 
{ withHG3DClass* `HG3DClass' } -> `()'  #}

{- function getBillboard -}
{#fun ogre_bbs_getBillboard as getBillboard 
{ withHG3DClass* `HG3DClass' ,
 fromIntegral `Int' ,
 alloca- `HG3DClass' peek*} -> `()'  #}

{- function removeBillboard -}
{#fun ogre_bbs_removeBillboard as removeBillboard 
{ withHG3DClass* `HG3DClass' ,
 fromIntegral `Int' } -> `()'  #}

{- function removeBillboard2 -}
{#fun ogre_bbs_removeBillboard2 as removeBillboard2 
{ withHG3DClass* `HG3DClass' ,
 withHG3DClass* `HG3DClass' } -> `()'  #}

{- function setBillboardOrigin -}
{#fun ogre_bbs_setBillboardOrigin as setBillboardOrigin 
{ withHG3DClass* `HG3DClass' ,
 cIntFromEnum `EnumBillboardOrigin' } -> `()'  #}

{- function getBillboardOrigin -}
{#fun ogre_bbs_getBillboardOrigin as getBillboardOrigin 
{ withHG3DClass* `HG3DClass' ,
 alloca- `EnumBillboardOrigin' peekEnumUtil*} -> `()'  #}

{- function setBillboardRotationType -}
{#fun ogre_bbs_setBillboardRotationType as setBillboardRotationType 
{ withHG3DClass* `HG3DClass' ,
 cIntFromEnum `EnumBillboardRotationType' } -> `()'  #}

{- function getBillboardRotationType -}
{#fun ogre_bbs_getBillboardRotationType as getBillboardRotationType 
{ withHG3DClass* `HG3DClass' ,
 alloca- `EnumBillboardRotationType' peekEnumUtil*} -> `()'  #}

{- function setDefaultDimensions -}
{#fun ogre_bbs_setDefaultDimensions as setDefaultDimensions 
{ withHG3DClass* `HG3DClass' ,
 realToFrac `Float' ,
 realToFrac `Float' } -> `()'  #}

{- function setDefaultWidth -}
{#fun ogre_bbs_setDefaultWidth as setDefaultWidth 
{ withHG3DClass* `HG3DClass' ,
 realToFrac `Float' } -> `()'  #}

{- function getDefaultWidth -}
{#fun ogre_bbs_getDefaultWidth as getDefaultWidth 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Float' peekFloatConv*} -> `()'  #}

{- function setDefaultHeight -}
{#fun ogre_bbs_setDefaultHeight as setDefaultHeight 
{ withHG3DClass* `HG3DClass' ,
 realToFrac `Float' } -> `()'  #}

{- function getDefaultHeight -}
{#fun ogre_bbs_getDefaultHeight as getDefaultHeight 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Float' peekFloatConv*} -> `()'  #}

{- function setMaterialName -}
{#fun ogre_bbs_setMaterialName as setMaterialName 
{ withHG3DClass* `HG3DClass' ,
 withCString* `String' ,
 withCString* `String' } -> `()'  #}

{- function getMaterialName -}
{#fun ogre_bbs_getMaterialName as getMaterialName 
{ withHG3DClass* `HG3DClass' ,
 alloc64k- `String' peekCString*} -> `()'  #}

{- function beginBillboards -}
{#fun ogre_bbs_beginBillboards as beginBillboards 
{ withHG3DClass* `HG3DClass' ,
 fromIntegral `Int' } -> `()'  #}

{- function injectBillboard -}
{#fun ogre_bbs_injectBillboard as injectBillboard 
{ withHG3DClass* `HG3DClass' ,
 withHG3DClass* `HG3DClass' } -> `()'  #}

{- function endBillboards -}
{#fun ogre_bbs_endBillboards as endBillboards 
{ withHG3DClass* `HG3DClass' } -> `()'  #}

{- function getBoundingRadius -}
{#fun ogre_bbs_getBoundingRadius as getBoundingRadius 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Float' peekFloatConv*} -> `()'  #}

{- function getMaterial -}
{#fun ogre_bbs_getMaterial as getMaterial 
{ withHG3DClass* `HG3DClass' ,
 alloca- `SharedPtr' peekSharedPtr*} -> `()'  #}

{- function setMaterial -}
{#fun ogre_bbs_setMaterial as setMaterial 
{ withHG3DClass* `HG3DClass' ,
 withSharedPtr* `SharedPtr' } -> `()'  #}

{- function getCullIndividually -}
{#fun ogre_bbs_getCullIndividually as getCullIndividually 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function setCullIndividually -}
{#fun ogre_bbs_setCullIndividually as setCullIndividually 
{ withHG3DClass* `HG3DClass' ,
 fromBool `Bool' } -> `()'  #}

{- function setBillboardType -}
{#fun ogre_bbs_setBillboardType as setBillboardType 
{ withHG3DClass* `HG3DClass' ,
 cIntFromEnum `EnumBillboardType' } -> `()'  #}

{- function getBillboardType -}
{#fun ogre_bbs_getBillboardType as getBillboardType 
{ withHG3DClass* `HG3DClass' ,
 alloca- `EnumBillboardType' peekEnumUtil*} -> `()'  #}

{- function setCommonDirection -}
{#fun ogre_bbs_setCommonDirection as setCommonDirection 
{ withHG3DClass* `HG3DClass' ,
 withVec3* `Vec3' } -> `()'  #}

{- function getCommonDirection -}
{#fun ogre_bbs_getCommonDirection as getCommonDirection 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Vec3' peekVec3*} -> `()'  #}

{- function setCommonUpVector -}
{#fun ogre_bbs_setCommonUpVector as setCommonUpVector 
{ withHG3DClass* `HG3DClass' ,
 withVec3* `Vec3' } -> `()'  #}

{- function getCommonUpVector -}
{#fun ogre_bbs_getCommonUpVector as getCommonUpVector 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Vec3' peekVec3*} -> `()'  #}

{- function setUseAccurateFacing -}
{#fun ogre_bbs_setUseAccurateFacing as setUseAccurateFacing 
{ withHG3DClass* `HG3DClass' ,
 fromBool `Bool' } -> `()'  #}

{- function getUseAccurateFacing -}
{#fun ogre_bbs_getUseAccurateFacing as getUseAccurateFacing 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function getMovableType -}
{#fun ogre_bbs_getMovableType as getMovableType 
{ withHG3DClass* `HG3DClass' ,
 alloc64k- `String' peekCString*} -> `()'  #}

{- function getSquaredViewDepth -}
{#fun ogre_bbs_getSquaredViewDepth as getSquaredViewDepth 
{ withHG3DClass* `HG3DClass' ,
 withHG3DClass* `HG3DClass' ,
 alloca- `Float' peekFloatConv*} -> `()'  #}

{- function setBillboardsInWorldSpace -}
{#fun ogre_bbs_setBillboardsInWorldSpace as setBillboardsInWorldSpace 
{ withHG3DClass* `HG3DClass' ,
 fromBool `Bool' } -> `()'  #}

{- function setPointRenderingEnabled -}
{#fun ogre_bbs_setPointRenderingEnabled as setPointRenderingEnabled 
{ withHG3DClass* `HG3DClass' ,
 fromBool `Bool' } -> `()'  #}

{- function isPointRenderingEnabled -}
{#fun ogre_bbs_isPointRenderingEnabled as isPointRenderingEnabled 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function getTypeFlags -}
{#fun ogre_bbs_getTypeFlags as getTypeFlags 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Int' peekIntConv*} -> `()'  #}

{- function setAutoUpdate -}
{#fun ogre_bbs_setAutoUpdate as setAutoUpdate 
{ withHG3DClass* `HG3DClass' ,
 fromBool `Bool' } -> `()'  #}

{- function getAutoUpdate -}
{#fun ogre_bbs_getAutoUpdate as getAutoUpdate 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function notifyBillboardDataChanged -}
{#fun ogre_bbs_notifyBillboardDataChanged as notifyBillboardDataChanged 
{ withHG3DClass* `HG3DClass' } -> `()'  #}

