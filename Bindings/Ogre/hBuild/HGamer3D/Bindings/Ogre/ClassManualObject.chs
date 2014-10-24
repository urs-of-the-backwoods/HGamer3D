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


-- ClassManualObject.chs

-- 

module HGamer3D.Bindings.Ogre.ClassManualObject where

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
{# import HGamer3D.Bindings.Ogre.EnumRenderOperationOperationType #}
{# import HGamer3D.Bindings.Ogre.StructVec3 #}
{# import HGamer3D.Bindings.Ogre.StructVec2 #}
{# import HGamer3D.Bindings.Ogre.StructColour #}
{# import HGamer3D.Bindings.Ogre.StructSharedPtr #}

#include "ClassManualObject.h"
{- function ManualObject -}
{#fun ogre_mno_construct as new 
{ withCString* `String' ,
 alloca- `HG3DClass' peek*} -> `()'  #}

{- function ~ManualObject -}
{#fun ogre_mno_destruct as delete 
{ withHG3DClass* `HG3DClass' } -> `()'  #}

{- function clear -}
{#fun ogre_mno_clear as clear 
{ withHG3DClass* `HG3DClass' } -> `()'  #}

{- function estimateVertexCount -}
{#fun ogre_mno_estimateVertexCount as estimateVertexCount 
{ withHG3DClass* `HG3DClass' ,
 fromIntegral `Int' } -> `()'  #}

{- function estimateIndexCount -}
{#fun ogre_mno_estimateIndexCount as estimateIndexCount 
{ withHG3DClass* `HG3DClass' ,
 fromIntegral `Int' } -> `()'  #}

{- function begin -}
{#fun ogre_mno_begin as begin 
{ withHG3DClass* `HG3DClass' ,
 withCString* `String' ,
 cIntFromEnum `EnumRenderOperationOperationType' ,
 withCString* `String' } -> `()'  #}

{- function setDynamic -}
{#fun ogre_mno_setDynamic as setDynamic 
{ withHG3DClass* `HG3DClass' ,
 fromBool `Bool' } -> `()'  #}

{- function getDynamic -}
{#fun ogre_mno_getDynamic as getDynamic 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function beginUpdate -}
{#fun ogre_mno_beginUpdate as beginUpdate 
{ withHG3DClass* `HG3DClass' ,
 fromIntegral `Int' } -> `()'  #}

{- function position -}
{#fun ogre_mno_position as position 
{ withHG3DClass* `HG3DClass' ,
 withVec3* `Vec3' } -> `()'  #}

{- function position2 -}
{#fun ogre_mno_position2 as position2 
{ withHG3DClass* `HG3DClass' ,
 realToFrac `Float' ,
 realToFrac `Float' ,
 realToFrac `Float' } -> `()'  #}

{- function normal -}
{#fun ogre_mno_normal as normal 
{ withHG3DClass* `HG3DClass' ,
 withVec3* `Vec3' } -> `()'  #}

{- function normal2 -}
{#fun ogre_mno_normal2 as normal2 
{ withHG3DClass* `HG3DClass' ,
 realToFrac `Float' ,
 realToFrac `Float' ,
 realToFrac `Float' } -> `()'  #}

{- function tangent -}
{#fun ogre_mno_tangent as tangent 
{ withHG3DClass* `HG3DClass' ,
 withVec3* `Vec3' } -> `()'  #}

{- function tangent2 -}
{#fun ogre_mno_tangent2 as tangent2 
{ withHG3DClass* `HG3DClass' ,
 realToFrac `Float' ,
 realToFrac `Float' ,
 realToFrac `Float' } -> `()'  #}

{- function textureCoord -}
{#fun ogre_mno_textureCoord as textureCoord 
{ withHG3DClass* `HG3DClass' ,
 realToFrac `Float' } -> `()'  #}

{- function textureCoord2 -}
{#fun ogre_mno_textureCoord2 as textureCoord2 
{ withHG3DClass* `HG3DClass' ,
 realToFrac `Float' ,
 realToFrac `Float' } -> `()'  #}

{- function textureCoord3 -}
{#fun ogre_mno_textureCoord3 as textureCoord3 
{ withHG3DClass* `HG3DClass' ,
 realToFrac `Float' ,
 realToFrac `Float' ,
 realToFrac `Float' } -> `()'  #}

{- function textureCoord4 -}
{#fun ogre_mno_textureCoord4 as textureCoord4 
{ withHG3DClass* `HG3DClass' ,
 realToFrac `Float' ,
 realToFrac `Float' ,
 realToFrac `Float' ,
 realToFrac `Float' } -> `()'  #}

{- function textureCoord5 -}
{#fun ogre_mno_textureCoord5 as textureCoord5 
{ withHG3DClass* `HG3DClass' ,
 withVec2* `Vec2' } -> `()'  #}

{- function textureCoord6 -}
{#fun ogre_mno_textureCoord6 as textureCoord6 
{ withHG3DClass* `HG3DClass' ,
 withVec3* `Vec3' } -> `()'  #}

{- function colour -}
{#fun ogre_mno_colour as colour 
{ withHG3DClass* `HG3DClass' ,
 withColour* `Colour' } -> `()'  #}

{- function colour2 -}
{#fun ogre_mno_colour2 as colour2 
{ withHG3DClass* `HG3DClass' ,
 realToFrac `Float' ,
 realToFrac `Float' ,
 realToFrac `Float' ,
 realToFrac `Float' } -> `()'  #}

{- function index -}
{#fun ogre_mno_index as index 
{ withHG3DClass* `HG3DClass' ,
 fromIntegral `Int' } -> `()'  #}

{- function triangle -}
{#fun ogre_mno_triangle as triangle 
{ withHG3DClass* `HG3DClass' ,
 fromIntegral `Int' ,
 fromIntegral `Int' ,
 fromIntegral `Int' } -> `()'  #}

{- function quad -}
{#fun ogre_mno_quad as quad 
{ withHG3DClass* `HG3DClass' ,
 fromIntegral `Int' ,
 fromIntegral `Int' ,
 fromIntegral `Int' ,
 fromIntegral `Int' } -> `()'  #}

{- function getCurrentVertexCount -}
{#fun ogre_mno_getCurrentVertexCount as getCurrentVertexCount 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Int' peekIntConv*} -> `()'  #}

{- function getCurrentIndexCount -}
{#fun ogre_mno_getCurrentIndexCount as getCurrentIndexCount 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Int' peekIntConv*} -> `()'  #}

{- function end -}
{#fun ogre_mno_end as end 
{ withHG3DClass* `HG3DClass' ,
 alloca- `HG3DClass' peek*} -> `()'  #}

{- function setMaterialName -}
{#fun ogre_mno_setMaterialName as setMaterialName 
{ withHG3DClass* `HG3DClass' ,
 fromIntegral `Int' ,
 withCString* `String' ,
 withCString* `String' } -> `()'  #}

{- function convertToMesh -}
{#fun ogre_mno_convertToMesh as convertToMesh 
{ withHG3DClass* `HG3DClass' ,
 withCString* `String' ,
 withCString* `String' ,
 alloca- `SharedPtr' peekSharedPtr*} -> `()'  #}

{- function setUseIdentityProjection -}
{#fun ogre_mno_setUseIdentityProjection as setUseIdentityProjection 
{ withHG3DClass* `HG3DClass' ,
 fromBool `Bool' } -> `()'  #}

{- function getUseIdentityProjection -}
{#fun ogre_mno_getUseIdentityProjection as getUseIdentityProjection 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function setUseIdentityView -}
{#fun ogre_mno_setUseIdentityView as setUseIdentityView 
{ withHG3DClass* `HG3DClass' ,
 fromBool `Bool' } -> `()'  #}

{- function getUseIdentityView -}
{#fun ogre_mno_getUseIdentityView as getUseIdentityView 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function getSection -}
{#fun ogre_mno_getSection as getSection 
{ withHG3DClass* `HG3DClass' ,
 fromIntegral `Int' ,
 alloca- `HG3DClass' peek*} -> `()'  #}

{- function getNumSections -}
{#fun ogre_mno_getNumSections as getNumSections 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Int' peekIntConv*} -> `()'  #}

{- function setKeepDeclarationOrder -}
{#fun ogre_mno_setKeepDeclarationOrder as setKeepDeclarationOrder 
{ withHG3DClass* `HG3DClass' ,
 fromBool `Bool' } -> `()'  #}

{- function getKeepDeclarationOrder -}
{#fun ogre_mno_getKeepDeclarationOrder as getKeepDeclarationOrder 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function getMovableType -}
{#fun ogre_mno_getMovableType as getMovableType 
{ withHG3DClass* `HG3DClass' ,
 alloc64k- `String' peekCString*} -> `()'  #}

{- function getBoundingRadius -}
{#fun ogre_mno_getBoundingRadius as getBoundingRadius 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Float' peekFloatConv*} -> `()'  #}

{- function hasEdgeList -}
{#fun ogre_mno_hasEdgeList as hasEdgeList 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

