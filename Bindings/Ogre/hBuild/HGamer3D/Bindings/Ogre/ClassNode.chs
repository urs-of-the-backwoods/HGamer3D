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


-- ClassNode.chs

-- 

module HGamer3D.Bindings.Ogre.ClassNode where

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
{# import HGamer3D.Bindings.Ogre.StructQuaternion #}
{# import HGamer3D.Bindings.Ogre.StructVec3 #}
{# import HGamer3D.Bindings.Ogre.EnumNodeTransformSpace #}
{# import HGamer3D.Bindings.Ogre.StructRadians #}

#include "ClassNode.h"
{- function ~Node -}
{#fun ogre_nd_destruct as delete 
{ withHG3DClass* `HG3DClass' } -> `()'  #}

{- function getName -}
{#fun ogre_nd_getName as getName 
{ withHG3DClass* `HG3DClass' ,
 alloc64k- `String' peekCString*} -> `()'  #}

{- function getParent -}
{#fun ogre_nd_getParent as getParent 
{ withHG3DClass* `HG3DClass' ,
 alloca- `HG3DClass' peek*} -> `()'  #}

{- function getOrientation -}
{#fun ogre_nd_getOrientation as getOrientation 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Quaternion' peekQuaternion*} -> `()'  #}

{- function setOrientation -}
{#fun ogre_nd_setOrientation as setOrientation 
{ withHG3DClass* `HG3DClass' ,
 withQuaternion* `Quaternion' } -> `()'  #}

{- function setOrientation2 -}
{#fun ogre_nd_setOrientation2 as setOrientation2 
{ withHG3DClass* `HG3DClass' ,
 realToFrac `Float' ,
 realToFrac `Float' ,
 realToFrac `Float' ,
 realToFrac `Float' } -> `()'  #}

{- function resetOrientation -}
{#fun ogre_nd_resetOrientation as resetOrientation 
{ withHG3DClass* `HG3DClass' } -> `()'  #}

{- function setPosition -}
{#fun ogre_nd_setPosition as setPosition 
{ withHG3DClass* `HG3DClass' ,
 withVec3* `Vec3' } -> `()'  #}

{- function setPosition2 -}
{#fun ogre_nd_setPosition2 as setPosition2 
{ withHG3DClass* `HG3DClass' ,
 realToFrac `Float' ,
 realToFrac `Float' ,
 realToFrac `Float' } -> `()'  #}

{- function getPosition -}
{#fun ogre_nd_getPosition as getPosition 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Vec3' peekVec3*} -> `()'  #}

{- function setScale -}
{#fun ogre_nd_setScale as setScale 
{ withHG3DClass* `HG3DClass' ,
 withVec3* `Vec3' } -> `()'  #}

{- function setScale2 -}
{#fun ogre_nd_setScale2 as setScale2 
{ withHG3DClass* `HG3DClass' ,
 realToFrac `Float' ,
 realToFrac `Float' ,
 realToFrac `Float' } -> `()'  #}

{- function getScale -}
{#fun ogre_nd_getScale as getScale 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Vec3' peekVec3*} -> `()'  #}

{- function setInheritOrientation -}
{#fun ogre_nd_setInheritOrientation as setInheritOrientation 
{ withHG3DClass* `HG3DClass' ,
 fromBool `Bool' } -> `()'  #}

{- function getInheritOrientation -}
{#fun ogre_nd_getInheritOrientation as getInheritOrientation 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function setInheritScale -}
{#fun ogre_nd_setInheritScale as setInheritScale 
{ withHG3DClass* `HG3DClass' ,
 fromBool `Bool' } -> `()'  #}

{- function getInheritScale -}
{#fun ogre_nd_getInheritScale as getInheritScale 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function scale -}
{#fun ogre_nd_scale as scale 
{ withHG3DClass* `HG3DClass' ,
 withVec3* `Vec3' } -> `()'  #}

{- function scale2 -}
{#fun ogre_nd_scale2 as scale2 
{ withHG3DClass* `HG3DClass' ,
 realToFrac `Float' ,
 realToFrac `Float' ,
 realToFrac `Float' } -> `()'  #}

{- function translate -}
{#fun ogre_nd_translate as translate 
{ withHG3DClass* `HG3DClass' ,
 withVec3* `Vec3' ,
 cIntFromEnum `EnumNodeTransformSpace' } -> `()'  #}

{- function translate2 -}
{#fun ogre_nd_translate2 as translate2 
{ withHG3DClass* `HG3DClass' ,
 realToFrac `Float' ,
 realToFrac `Float' ,
 realToFrac `Float' ,
 cIntFromEnum `EnumNodeTransformSpace' } -> `()'  #}

{- function roll -}
{#fun ogre_nd_roll as roll 
{ withHG3DClass* `HG3DClass' ,
 withRadians* `Radians' ,
 cIntFromEnum `EnumNodeTransformSpace' } -> `()'  #}

{- function pitch -}
{#fun ogre_nd_pitch as pitch 
{ withHG3DClass* `HG3DClass' ,
 withRadians* `Radians' ,
 cIntFromEnum `EnumNodeTransformSpace' } -> `()'  #}

{- function yaw -}
{#fun ogre_nd_yaw as yaw 
{ withHG3DClass* `HG3DClass' ,
 withRadians* `Radians' ,
 cIntFromEnum `EnumNodeTransformSpace' } -> `()'  #}

{- function rotate -}
{#fun ogre_nd_rotate as rotate 
{ withHG3DClass* `HG3DClass' ,
 withVec3* `Vec3' ,
 withRadians* `Radians' ,
 cIntFromEnum `EnumNodeTransformSpace' } -> `()'  #}

{- function rotate2 -}
{#fun ogre_nd_rotate2 as rotate2 
{ withHG3DClass* `HG3DClass' ,
 withQuaternion* `Quaternion' ,
 cIntFromEnum `EnumNodeTransformSpace' } -> `()'  #}

{- function createChild -}
{#fun ogre_nd_createChild as createChild 
{ withHG3DClass* `HG3DClass' ,
 withVec3* `Vec3' ,
 withQuaternion* `Quaternion' ,
 alloca- `HG3DClass' peek*} -> `()'  #}

{- function createChild2 -}
{#fun ogre_nd_createChild2 as createChild2 
{ withHG3DClass* `HG3DClass' ,
 withCString* `String' ,
 withVec3* `Vec3' ,
 withQuaternion* `Quaternion' ,
 alloca- `HG3DClass' peek*} -> `()'  #}

{- function addChild -}
{#fun ogre_nd_addChild as addChild 
{ withHG3DClass* `HG3DClass' ,
 withHG3DClass* `HG3DClass' } -> `()'  #}

{- function numChildren -}
{#fun ogre_nd_numChildren as numChildren 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Int' peekIntConv*} -> `()'  #}

{- function getChild -}
{#fun ogre_nd_getChild as getChild 
{ withHG3DClass* `HG3DClass' ,
 fromIntegral `Int' ,
 alloca- `HG3DClass' peek*} -> `()'  #}

{- function getChild2 -}
{#fun ogre_nd_getChild2 as getChild2 
{ withHG3DClass* `HG3DClass' ,
 withCString* `String' ,
 alloca- `HG3DClass' peek*} -> `()'  #}

{- function removeChild -}
{#fun ogre_nd_removeChild as removeChild 
{ withHG3DClass* `HG3DClass' ,
 fromIntegral `Int' ,
 alloca- `HG3DClass' peek*} -> `()'  #}

{- function removeChild2 -}
{#fun ogre_nd_removeChild2 as removeChild2 
{ withHG3DClass* `HG3DClass' ,
 withHG3DClass* `HG3DClass' ,
 alloca- `HG3DClass' peek*} -> `()'  #}

{- function removeChild3 -}
{#fun ogre_nd_removeChild3 as removeChild3 
{ withHG3DClass* `HG3DClass' ,
 withCString* `String' ,
 alloca- `HG3DClass' peek*} -> `()'  #}

{- function removeAllChildren -}
{#fun ogre_nd_removeAllChildren as removeAllChildren 
{ withHG3DClass* `HG3DClass' } -> `()'  #}

{- function setInitialState -}
{#fun ogre_nd_setInitialState as setInitialState 
{ withHG3DClass* `HG3DClass' } -> `()'  #}

{- function resetToInitialState -}
{#fun ogre_nd_resetToInitialState as resetToInitialState 
{ withHG3DClass* `HG3DClass' } -> `()'  #}

{- function getInitialPosition -}
{#fun ogre_nd_getInitialPosition as getInitialPosition 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Vec3' peekVec3*} -> `()'  #}

{- function convertWorldToLocalPosition -}
{#fun ogre_nd_convertWorldToLocalPosition as convertWorldToLocalPosition 
{ withHG3DClass* `HG3DClass' ,
 withVec3* `Vec3' ,
 alloca- `Vec3' peekVec3*} -> `()'  #}

{- function convertLocalToWorldPosition -}
{#fun ogre_nd_convertLocalToWorldPosition as convertLocalToWorldPosition 
{ withHG3DClass* `HG3DClass' ,
 withVec3* `Vec3' ,
 alloca- `Vec3' peekVec3*} -> `()'  #}

{- function convertWorldToLocalOrientation -}
{#fun ogre_nd_convertWorldToLocalOrientation as convertWorldToLocalOrientation 
{ withHG3DClass* `HG3DClass' ,
 withQuaternion* `Quaternion' ,
 alloca- `Quaternion' peekQuaternion*} -> `()'  #}

{- function convertLocalToWorldOrientation -}
{#fun ogre_nd_convertLocalToWorldOrientation as convertLocalToWorldOrientation 
{ withHG3DClass* `HG3DClass' ,
 withQuaternion* `Quaternion' ,
 alloca- `Quaternion' peekQuaternion*} -> `()'  #}

{- function getInitialOrientation -}
{#fun ogre_nd_getInitialOrientation as getInitialOrientation 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Quaternion' peekQuaternion*} -> `()'  #}

{- function getInitialScale -}
{#fun ogre_nd_getInitialScale as getInitialScale 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Vec3' peekVec3*} -> `()'  #}

{- function getSquaredViewDepth -}
{#fun ogre_nd_getSquaredViewDepth as getSquaredViewDepth 
{ withHG3DClass* `HG3DClass' ,
 withHG3DClass* `HG3DClass' ,
 alloca- `Float' peekFloatConv*} -> `()'  #}

{- function needUpdate -}
{#fun ogre_nd_needUpdate as needUpdate 
{ withHG3DClass* `HG3DClass' ,
 fromBool `Bool' } -> `()'  #}

{- function requestUpdate -}
{#fun ogre_nd_requestUpdate as requestUpdate 
{ withHG3DClass* `HG3DClass' ,
 withHG3DClass* `HG3DClass' ,
 fromBool `Bool' } -> `()'  #}

{- function cancelUpdate -}
{#fun ogre_nd_cancelUpdate as cancelUpdate 
{ withHG3DClass* `HG3DClass' ,
 withHG3DClass* `HG3DClass' } -> `()'  #}

{- function queueNeedUpdate -}
{#fun ogre_nd_queueNeedUpdate as queueNeedUpdate 
{ withHG3DClass* `HG3DClass' } -> `()'  #}

{- function processQueuedUpdates -}
{#fun ogre_nd_processQueuedUpdates as processQueuedUpdates 
{ } -> `()'  #}

