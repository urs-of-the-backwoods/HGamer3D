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


-- ClassResourceManager.chs

-- 

module HGamer3D.Bindings.Ogre.ClassResourceManager where

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

#include "ClassResourceManager.h"
{- function ~ResourceManager -}
{#fun ogre_rsrcmgr_destruct as delete 
{ withHG3DClass* `HG3DClass' } -> `()'  #}

{- function setMemoryBudget -}
{#fun ogre_rsrcmgr_setMemoryBudget as setMemoryBudget 
{ withHG3DClass* `HG3DClass' ,
 fromIntegral `Int' } -> `()'  #}

{- function getMemoryBudget -}
{#fun ogre_rsrcmgr_getMemoryBudget as getMemoryBudget 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Int' peekIntConv*} -> `()'  #}

{- function getMemoryUsage -}
{#fun ogre_rsrcmgr_getMemoryUsage as getMemoryUsage 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Int' peekIntConv*} -> `()'  #}

{- function unload -}
{#fun ogre_rsrcmgr_unload as unload 
{ withHG3DClass* `HG3DClass' ,
 withCString* `String' } -> `()'  #}

{- function unloadAll -}
{#fun ogre_rsrcmgr_unloadAll as unloadAll 
{ withHG3DClass* `HG3DClass' ,
 fromBool `Bool' } -> `()'  #}

{- function reloadAll -}
{#fun ogre_rsrcmgr_reloadAll as reloadAll 
{ withHG3DClass* `HG3DClass' ,
 fromBool `Bool' } -> `()'  #}

{- function unloadUnreferencedResources -}
{#fun ogre_rsrcmgr_unloadUnreferencedResources as unloadUnreferencedResources 
{ withHG3DClass* `HG3DClass' ,
 fromBool `Bool' } -> `()'  #}

{- function reloadUnreferencedResources -}
{#fun ogre_rsrcmgr_reloadUnreferencedResources as reloadUnreferencedResources 
{ withHG3DClass* `HG3DClass' ,
 fromBool `Bool' } -> `()'  #}

{- function remove2 -}
{#fun ogre_rsrcmgr_remove2 as remove2 
{ withHG3DClass* `HG3DClass' ,
 withCString* `String' } -> `()'  #}

{- function removeAll -}
{#fun ogre_rsrcmgr_removeAll as removeAll 
{ withHG3DClass* `HG3DClass' } -> `()'  #}

{- function removeUnreferencedResources -}
{#fun ogre_rsrcmgr_removeUnreferencedResources as removeUnreferencedResources 
{ withHG3DClass* `HG3DClass' ,
 fromBool `Bool' } -> `()'  #}

{- function resourceExists -}
{#fun ogre_rsrcmgr_resourceExists as resourceExists 
{ withHG3DClass* `HG3DClass' ,
 withCString* `String' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function getLoadingOrder -}
{#fun ogre_rsrcmgr_getLoadingOrder as getLoadingOrder 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Float' peekFloatConv*} -> `()'  #}

{- function getResourceType -}
{#fun ogre_rsrcmgr_getResourceType as getResourceType 
{ withHG3DClass* `HG3DClass' ,
 alloc64k- `String' peekCString*} -> `()'  #}

{- function setVerbose -}
{#fun ogre_rsrcmgr_setVerbose as setVerbose 
{ withHG3DClass* `HG3DClass' ,
 fromBool `Bool' } -> `()'  #}

{- function getVerbose -}
{#fun ogre_rsrcmgr_getVerbose as getVerbose 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function destroyResourcePool2 -}
{#fun ogre_rsrcmgr_destroyResourcePool2 as destroyResourcePool2 
{ withHG3DClass* `HG3DClass' ,
 withCString* `String' } -> `()'  #}

{- function destroyAllResourcePools -}
{#fun ogre_rsrcmgr_destroyAllResourcePools as destroyAllResourcePools 
{ withHG3DClass* `HG3DClass' } -> `()'  #}

