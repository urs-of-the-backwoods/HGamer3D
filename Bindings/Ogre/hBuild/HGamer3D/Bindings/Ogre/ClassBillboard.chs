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


-- ClassBillboard.chs

-- 

module HGamer3D.Bindings.Ogre.ClassBillboard where

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
{# import HGamer3D.Bindings.Ogre.StructRadians #}
{# import HGamer3D.Bindings.Ogre.StructVec3 #}
{# import HGamer3D.Bindings.Ogre.StructColour #}

#include "ClassBillboard.h"
{- function Billboard -}
{#fun ogre_bbd_construct as new 
{ alloca- `HG3DClass' peek*} -> `()'  #}

{- function ~Billboard -}
{#fun ogre_bbd_destruct as delete 
{ withHG3DClass* `HG3DClass' } -> `()'  #}

{- function getRotation -}
{#fun ogre_bbd_getRotation as getRotation 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Radians' peekRadians*} -> `()'  #}

{- function setRotation -}
{#fun ogre_bbd_setRotation as setRotation 
{ withHG3DClass* `HG3DClass' ,
 withRadians* `Radians' } -> `()'  #}

{- function setPosition -}
{#fun ogre_bbd_setPosition as setPosition 
{ withHG3DClass* `HG3DClass' ,
 withVec3* `Vec3' } -> `()'  #}

{- function setPosition2 -}
{#fun ogre_bbd_setPosition2 as setPosition2 
{ withHG3DClass* `HG3DClass' ,
 realToFrac `Float' ,
 realToFrac `Float' ,
 realToFrac `Float' } -> `()'  #}

{- function getPosition -}
{#fun ogre_bbd_getPosition as getPosition 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Vec3' peekVec3*} -> `()'  #}

{- function setDimensions -}
{#fun ogre_bbd_setDimensions as setDimensions 
{ withHG3DClass* `HG3DClass' ,
 realToFrac `Float' ,
 realToFrac `Float' } -> `()'  #}

{- function resetDimensions -}
{#fun ogre_bbd_resetDimensions as resetDimensions 
{ withHG3DClass* `HG3DClass' } -> `()'  #}

{- function setColour -}
{#fun ogre_bbd_setColour as setColour 
{ withHG3DClass* `HG3DClass' ,
 withColour* `Colour' } -> `()'  #}

{- function getColour -}
{#fun ogre_bbd_getColour as getColour 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Colour' peekColour*} -> `()'  #}

{- function hasOwnDimensions -}
{#fun ogre_bbd_hasOwnDimensions as hasOwnDimensions 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function getOwnWidth -}
{#fun ogre_bbd_getOwnWidth as getOwnWidth 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Float' peekFloatConv*} -> `()'  #}

{- function getOwnHeight -}
{#fun ogre_bbd_getOwnHeight as getOwnHeight 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Float' peekFloatConv*} -> `()'  #}

{- function isUseTexcoordRect -}
{#fun ogre_bbd_isUseTexcoordRect as isUseTexcoordRect 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function setTexcoordIndex -}
{#fun ogre_bbd_setTexcoordIndex as setTexcoordIndex 
{ withHG3DClass* `HG3DClass' ,
 fromIntegral `Int' } -> `()'  #}

{- function getTexcoordIndex -}
{#fun ogre_bbd_getTexcoordIndex as getTexcoordIndex 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Int' peekIntConv*} -> `()'  #}

{- function setTexcoordRect2 -}
{#fun ogre_bbd_setTexcoordRect2 as setTexcoordRect2 
{ withHG3DClass* `HG3DClass' ,
 realToFrac `Float' ,
 realToFrac `Float' ,
 realToFrac `Float' ,
 realToFrac `Float' } -> `()'  #}

