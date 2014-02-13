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


-- ClassResource.chs

-- 

module HGamer3D.Bindings.Ogre.ClassResource where

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

#include "ClassResource.h"
{- function ~Resource -}
{#fun ogre_rsrc_destruct as delete 
{ withHG3DClass* `HG3DClass' } -> `()'  #}

{- function prepare -}
{#fun ogre_rsrc_prepare as prepare 
{ withHG3DClass* `HG3DClass' ,
 fromBool `Bool' } -> `()'  #}

{- function load -}
{#fun ogre_rsrc_load as load 
{ withHG3DClass* `HG3DClass' ,
 fromBool `Bool' } -> `()'  #}

{- function reload -}
{#fun ogre_rsrc_reload as reload 
{ withHG3DClass* `HG3DClass' } -> `()'  #}

{- function isReloadable -}
{#fun ogre_rsrc_isReloadable as isReloadable 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function isManuallyLoaded -}
{#fun ogre_rsrc_isManuallyLoaded as isManuallyLoaded 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function unload -}
{#fun ogre_rsrc_unload as unload 
{ withHG3DClass* `HG3DClass' } -> `()'  #}

{- function getSize -}
{#fun ogre_rsrc_getSize as getSize 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Int' peekIntConv*} -> `()'  #}

{- function touch -}
{#fun ogre_rsrc_touch as touch 
{ withHG3DClass* `HG3DClass' } -> `()'  #}

{- function getName -}
{#fun ogre_rsrc_getName as getName 
{ withHG3DClass* `HG3DClass' ,
 alloc64k- `String' peekCString*} -> `()'  #}

{- function isPrepared -}
{#fun ogre_rsrc_isPrepared as isPrepared 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function isLoaded -}
{#fun ogre_rsrc_isLoaded as isLoaded 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function isLoading -}
{#fun ogre_rsrc_isLoading as isLoading 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function isBackgroundLoaded -}
{#fun ogre_rsrc_isBackgroundLoaded as isBackgroundLoaded 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function setBackgroundLoaded -}
{#fun ogre_rsrc_setBackgroundLoaded as setBackgroundLoaded 
{ withHG3DClass* `HG3DClass' ,
 fromBool `Bool' } -> `()'  #}

{- function escalateLoading -}
{#fun ogre_rsrc_escalateLoading as escalateLoading 
{ withHG3DClass* `HG3DClass' } -> `()'  #}

{- function getGroup -}
{#fun ogre_rsrc_getGroup as getGroup 
{ withHG3DClass* `HG3DClass' ,
 alloc64k- `String' peekCString*} -> `()'  #}

{- function changeGroupOwnership -}
{#fun ogre_rsrc_changeGroupOwnership as changeGroupOwnership 
{ withHG3DClass* `HG3DClass' ,
 withCString* `String' } -> `()'  #}

{- function getCreator -}
{#fun ogre_rsrc_getCreator as getCreator 
{ withHG3DClass* `HG3DClass' ,
 alloca- `HG3DClass' peek*} -> `()'  #}

{- function getOrigin -}
{#fun ogre_rsrc_getOrigin as getOrigin 
{ withHG3DClass* `HG3DClass' ,
 alloc64k- `String' peekCString*} -> `()'  #}

{- function getStateCount -}
{#fun ogre_rsrc_getStateCount as getStateCount 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Int' peekIntConv*} -> `()'  #}

