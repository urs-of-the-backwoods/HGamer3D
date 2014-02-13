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


-- ClassMaterial.chs

-- 

module HGamer3D.Bindings.Ogre.ClassMaterial where

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
{# import HGamer3D.Bindings.Ogre.StructSharedPtr #}
{# import HGamer3D.Bindings.Ogre.StructColour #}

#include "ClassMaterial.h"
{- function ~Material -}
{#fun ogre_mtrl_destruct as delete 
{ withHG3DClass* `HG3DClass' } -> `()'  #}

{- function isTransparent -}
{#fun ogre_mtrl_isTransparent as isTransparent 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function setReceiveShadows -}
{#fun ogre_mtrl_setReceiveShadows as setReceiveShadows 
{ withHG3DClass* `HG3DClass' ,
 fromBool `Bool' } -> `()'  #}

{- function getReceiveShadows -}
{#fun ogre_mtrl_getReceiveShadows as getReceiveShadows 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function setTransparencyCastsShadows -}
{#fun ogre_mtrl_setTransparencyCastsShadows as setTransparencyCastsShadows 
{ withHG3DClass* `HG3DClass' ,
 fromBool `Bool' } -> `()'  #}

{- function getTransparencyCastsShadows -}
{#fun ogre_mtrl_getTransparencyCastsShadows as getTransparencyCastsShadows 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function getNumTechniques -}
{#fun ogre_mtrl_getNumTechniques as getNumTechniques 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Int' peekIntConv*} -> `()'  #}

{- function removeTechnique -}
{#fun ogre_mtrl_removeTechnique as removeTechnique 
{ withHG3DClass* `HG3DClass' ,
 fromIntegral `Int' } -> `()'  #}

{- function removeAllTechniques -}
{#fun ogre_mtrl_removeAllTechniques as removeAllTechniques 
{ withHG3DClass* `HG3DClass' } -> `()'  #}

{- function getNumSupportedTechniques -}
{#fun ogre_mtrl_getNumSupportedTechniques as getNumSupportedTechniques 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Int' peekIntConv*} -> `()'  #}

{- function getUnsupportedTechniquesExplanation -}
{#fun ogre_mtrl_getUnsupportedTechniquesExplanation as getUnsupportedTechniquesExplanation 
{ withHG3DClass* `HG3DClass' ,
 alloc64k- `String' peekCString*} -> `()'  #}

{- function getNumLodLevels -}
{#fun ogre_mtrl_getNumLodLevels as getNumLodLevels 
{ withHG3DClass* `HG3DClass' ,
 fromIntegral `Int' ,
 alloca- `Int' peekIntConv*} -> `()'  #}

{- function getNumLodLevels2 -}
{#fun ogre_mtrl_getNumLodLevels2 as getNumLodLevels2 
{ withHG3DClass* `HG3DClass' ,
 withCString* `String' ,
 alloca- `Int' peekIntConv*} -> `()'  #}

{- function clone -}
{#fun ogre_mtrl_clone as clone 
{ withHG3DClass* `HG3DClass' ,
 withCString* `String' ,
 fromBool `Bool' ,
 withCString* `String' ,
 alloca- `SharedPtr' peekSharedPtr*} -> `()'  #}

{- function copyDetailsTo -}
{#fun ogre_mtrl_copyDetailsTo as copyDetailsTo 
{ withHG3DClass* `HG3DClass' ,
 alloca- `SharedPtr' peekSharedPtr*} -> `()'  #}

{- function compile -}
{#fun ogre_mtrl_compile as compile 
{ withHG3DClass* `HG3DClass' ,
 fromBool `Bool' } -> `()'  #}

{- function setPointSize -}
{#fun ogre_mtrl_setPointSize as setPointSize 
{ withHG3DClass* `HG3DClass' ,
 realToFrac `Float' } -> `()'  #}

{- function setAmbient -}
{#fun ogre_mtrl_setAmbient as setAmbient 
{ withHG3DClass* `HG3DClass' ,
 realToFrac `Float' ,
 realToFrac `Float' ,
 realToFrac `Float' } -> `()'  #}

{- function setAmbient2 -}
{#fun ogre_mtrl_setAmbient2 as setAmbient2 
{ withHG3DClass* `HG3DClass' ,
 withColour* `Colour' } -> `()'  #}

{- function setDiffuse -}
{#fun ogre_mtrl_setDiffuse as setDiffuse 
{ withHG3DClass* `HG3DClass' ,
 realToFrac `Float' ,
 realToFrac `Float' ,
 realToFrac `Float' ,
 realToFrac `Float' } -> `()'  #}

{- function setDiffuse2 -}
{#fun ogre_mtrl_setDiffuse2 as setDiffuse2 
{ withHG3DClass* `HG3DClass' ,
 withColour* `Colour' } -> `()'  #}

{- function setSpecular -}
{#fun ogre_mtrl_setSpecular as setSpecular 
{ withHG3DClass* `HG3DClass' ,
 realToFrac `Float' ,
 realToFrac `Float' ,
 realToFrac `Float' ,
 realToFrac `Float' } -> `()'  #}

{- function setSpecular2 -}
{#fun ogre_mtrl_setSpecular2 as setSpecular2 
{ withHG3DClass* `HG3DClass' ,
 withColour* `Colour' } -> `()'  #}

{- function setShininess -}
{#fun ogre_mtrl_setShininess as setShininess 
{ withHG3DClass* `HG3DClass' ,
 realToFrac `Float' } -> `()'  #}

{- function setSelfIllumination -}
{#fun ogre_mtrl_setSelfIllumination as setSelfIllumination 
{ withHG3DClass* `HG3DClass' ,
 realToFrac `Float' ,
 realToFrac `Float' ,
 realToFrac `Float' } -> `()'  #}

{- function setSelfIllumination2 -}
{#fun ogre_mtrl_setSelfIllumination2 as setSelfIllumination2 
{ withHG3DClass* `HG3DClass' ,
 withColour* `Colour' } -> `()'  #}

{- function setDepthCheckEnabled -}
{#fun ogre_mtrl_setDepthCheckEnabled as setDepthCheckEnabled 
{ withHG3DClass* `HG3DClass' ,
 fromBool `Bool' } -> `()'  #}

{- function setDepthWriteEnabled -}
{#fun ogre_mtrl_setDepthWriteEnabled as setDepthWriteEnabled 
{ withHG3DClass* `HG3DClass' ,
 fromBool `Bool' } -> `()'  #}

{- function setColourWriteEnabled -}
{#fun ogre_mtrl_setColourWriteEnabled as setColourWriteEnabled 
{ withHG3DClass* `HG3DClass' ,
 fromBool `Bool' } -> `()'  #}

{- function setLightingEnabled -}
{#fun ogre_mtrl_setLightingEnabled as setLightingEnabled 
{ withHG3DClass* `HG3DClass' ,
 fromBool `Bool' } -> `()'  #}

{- function setDepthBias -}
{#fun ogre_mtrl_setDepthBias as setDepthBias 
{ withHG3DClass* `HG3DClass' ,
 realToFrac `Float' ,
 realToFrac `Float' } -> `()'  #}

{- function setTextureAnisotropy -}
{#fun ogre_mtrl_setTextureAnisotropy as setTextureAnisotropy 
{ withHG3DClass* `HG3DClass' ,
 fromIntegral `Int' } -> `()'  #}

{- function touch -}
{#fun ogre_mtrl_touch as touch 
{ withHG3DClass* `HG3DClass' } -> `()'  #}

{- function getCompilationRequired -}
{#fun ogre_mtrl_getCompilationRequired as getCompilationRequired 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

