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


-- ClassResourceGroupManager.chs

-- 

module HGamer3D.Bindings.Ogre.ClassResourceGroupManager where

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

#include "ClassResourceGroupManager.h"
{- function ResourceGroupManager -}
{#fun ogre_rgmgr_construct as new 
{ alloca- `HG3DClass' peek*} -> `()'  #}

{- function ~ResourceGroupManager -}
{#fun ogre_rgmgr_destruct as delete 
{ withHG3DClass* `HG3DClass' } -> `()'  #}

{- function createResourceGroup -}
{#fun ogre_rgmgr_createResourceGroup as createResourceGroup 
{ withHG3DClass* `HG3DClass' ,
 withCString* `String' ,
 fromBool `Bool' } -> `()'  #}

{- function initialiseResourceGroup -}
{#fun ogre_rgmgr_initialiseResourceGroup as initialiseResourceGroup 
{ withHG3DClass* `HG3DClass' ,
 withCString* `String' } -> `()'  #}

{- function initialiseAllResourceGroups -}
{#fun ogre_rgmgr_initialiseAllResourceGroups as initialiseAllResourceGroups 
{ withHG3DClass* `HG3DClass' } -> `()'  #}

{- function prepareResourceGroup -}
{#fun ogre_rgmgr_prepareResourceGroup as prepareResourceGroup 
{ withHG3DClass* `HG3DClass' ,
 withCString* `String' ,
 fromBool `Bool' ,
 fromBool `Bool' } -> `()'  #}

{- function loadResourceGroup -}
{#fun ogre_rgmgr_loadResourceGroup as loadResourceGroup 
{ withHG3DClass* `HG3DClass' ,
 withCString* `String' ,
 fromBool `Bool' ,
 fromBool `Bool' } -> `()'  #}

{- function unloadResourceGroup -}
{#fun ogre_rgmgr_unloadResourceGroup as unloadResourceGroup 
{ withHG3DClass* `HG3DClass' ,
 withCString* `String' ,
 fromBool `Bool' } -> `()'  #}

{- function unloadUnreferencedResourcesInGroup -}
{#fun ogre_rgmgr_unloadUnreferencedResourcesInGroup as unloadUnreferencedResourcesInGroup 
{ withHG3DClass* `HG3DClass' ,
 withCString* `String' ,
 fromBool `Bool' } -> `()'  #}

{- function clearResourceGroup -}
{#fun ogre_rgmgr_clearResourceGroup as clearResourceGroup 
{ withHG3DClass* `HG3DClass' ,
 withCString* `String' } -> `()'  #}

{- function destroyResourceGroup -}
{#fun ogre_rgmgr_destroyResourceGroup as destroyResourceGroup 
{ withHG3DClass* `HG3DClass' ,
 withCString* `String' } -> `()'  #}

{- function isResourceGroupInitialised -}
{#fun ogre_rgmgr_isResourceGroupInitialised as isResourceGroupInitialised 
{ withHG3DClass* `HG3DClass' ,
 withCString* `String' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function isResourceGroupLoaded -}
{#fun ogre_rgmgr_isResourceGroupLoaded as isResourceGroupLoaded 
{ withHG3DClass* `HG3DClass' ,
 withCString* `String' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function resourceGroupExists -}
{#fun ogre_rgmgr_resourceGroupExists as resourceGroupExists 
{ withHG3DClass* `HG3DClass' ,
 withCString* `String' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function addResourceLocation -}
{#fun ogre_rgmgr_addResourceLocation as addResourceLocation 
{ withHG3DClass* `HG3DClass' ,
 withCString* `String' ,
 withCString* `String' ,
 withCString* `String' ,
 fromBool `Bool' } -> `()'  #}

{- function removeResourceLocation -}
{#fun ogre_rgmgr_removeResourceLocation as removeResourceLocation 
{ withHG3DClass* `HG3DClass' ,
 withCString* `String' ,
 withCString* `String' } -> `()'  #}

{- function resourceLocationExists -}
{#fun ogre_rgmgr_resourceLocationExists as resourceLocationExists 
{ withHG3DClass* `HG3DClass' ,
 withCString* `String' ,
 withCString* `String' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function undeclareResource -}
{#fun ogre_rgmgr_undeclareResource as undeclareResource 
{ withHG3DClass* `HG3DClass' ,
 withCString* `String' ,
 withCString* `String' } -> `()'  #}

{- function resourceExists -}
{#fun ogre_rgmgr_resourceExists as resourceExists 
{ withHG3DClass* `HG3DClass' ,
 withCString* `String' ,
 withCString* `String' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function resourceExistsInAnyGroup -}
{#fun ogre_rgmgr_resourceExistsInAnyGroup as resourceExistsInAnyGroup 
{ withHG3DClass* `HG3DClass' ,
 withCString* `String' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function findGroupContainingResource -}
{#fun ogre_rgmgr_findGroupContainingResource as findGroupContainingResource 
{ withHG3DClass* `HG3DClass' ,
 withCString* `String' ,
 alloc64k- `String' peekCString*} -> `()'  #}

{- function deleteResource -}
{#fun ogre_rgmgr_deleteResource as deleteResource 
{ withHG3DClass* `HG3DClass' ,
 withCString* `String' ,
 withCString* `String' ,
 withCString* `String' } -> `()'  #}

{- function deleteMatchingResources -}
{#fun ogre_rgmgr_deleteMatchingResources as deleteMatchingResources 
{ withHG3DClass* `HG3DClass' ,
 withCString* `String' ,
 withCString* `String' ,
 withCString* `String' } -> `()'  #}

{- function setWorldResourceGroupName -}
{#fun ogre_rgmgr_setWorldResourceGroupName as setWorldResourceGroupName 
{ withHG3DClass* `HG3DClass' ,
 withCString* `String' } -> `()'  #}

{- function getWorldResourceGroupName -}
{#fun ogre_rgmgr_getWorldResourceGroupName as getWorldResourceGroupName 
{ withHG3DClass* `HG3DClass' ,
 alloc64k- `String' peekCString*} -> `()'  #}

{- function linkWorldGeometryToResourceGroup -}
{#fun ogre_rgmgr_linkWorldGeometryToResourceGroup as linkWorldGeometryToResourceGroup 
{ withHG3DClass* `HG3DClass' ,
 withCString* `String' ,
 withCString* `String' ,
 withHG3DClass* `HG3DClass' } -> `()'  #}

{- function unlinkWorldGeometryFromResourceGroup -}
{#fun ogre_rgmgr_unlinkWorldGeometryFromResourceGroup as unlinkWorldGeometryFromResourceGroup 
{ withHG3DClass* `HG3DClass' ,
 withCString* `String' } -> `()'  #}

{- function isResourceGroupInGlobalPool -}
{#fun ogre_rgmgr_isResourceGroupInGlobalPool as isResourceGroupInGlobalPool 
{ withHG3DClass* `HG3DClass' ,
 withCString* `String' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function shutdownAll -}
{#fun ogre_rgmgr_shutdownAll as shutdownAll 
{ withHG3DClass* `HG3DClass' } -> `()'  #}

{- function getSingleton -}
{#fun ogre_rgmgr_getSingleton as getSingleton 
{ alloca- `HG3DClass' peek*} -> `()'  #}

{- function getSingletonPtr -}
{#fun ogre_rgmgr_getSingletonPtr as getSingletonPtr 
{ alloca- `HG3DClass' peek*} -> `()'  #}

