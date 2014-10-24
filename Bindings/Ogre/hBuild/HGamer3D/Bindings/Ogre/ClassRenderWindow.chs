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


-- ClassRenderWindow.chs

-- 

module HGamer3D.Bindings.Ogre.ClassRenderWindow where

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

#include "ClassRenderWindow.h"
{- function setFullscreen -}
{#fun ogre_rw_setFullscreen as setFullscreen 
{ withHG3DClass* `HG3DClass' ,
 fromBool `Bool' ,
 fromIntegral `Int' ,
 fromIntegral `Int' } -> `()'  #}

{- function destroy -}
{#fun ogre_rw_destroy as destroy 
{ withHG3DClass* `HG3DClass' } -> `()'  #}

{- function resize -}
{#fun ogre_rw_resize as resize 
{ withHG3DClass* `HG3DClass' ,
 fromIntegral `Int' ,
 fromIntegral `Int' } -> `()'  #}

{- function windowMovedOrResized -}
{#fun ogre_rw_windowMovedOrResized as windowMovedOrResized 
{ withHG3DClass* `HG3DClass' } -> `()'  #}

{- function reposition -}
{#fun ogre_rw_reposition as reposition 
{ withHG3DClass* `HG3DClass' ,
 fromIntegral `Int' ,
 fromIntegral `Int' } -> `()'  #}

{- function isVisible -}
{#fun ogre_rw_isVisible as isVisible 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function setVisible -}
{#fun ogre_rw_setVisible as setVisible 
{ withHG3DClass* `HG3DClass' ,
 fromBool `Bool' } -> `()'  #}

{- function isHidden -}
{#fun ogre_rw_isHidden as isHidden 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function setHidden -}
{#fun ogre_rw_setHidden as setHidden 
{ withHG3DClass* `HG3DClass' ,
 fromBool `Bool' } -> `()'  #}

{- function setVSyncEnabled -}
{#fun ogre_rw_setVSyncEnabled as setVSyncEnabled 
{ withHG3DClass* `HG3DClass' ,
 fromBool `Bool' } -> `()'  #}

{- function isVSyncEnabled -}
{#fun ogre_rw_isVSyncEnabled as isVSyncEnabled 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function setVSyncInterval -}
{#fun ogre_rw_setVSyncInterval as setVSyncInterval 
{ withHG3DClass* `HG3DClass' ,
 fromIntegral `Int' } -> `()'  #}

{- function getVSyncInterval -}
{#fun ogre_rw_getVSyncInterval as getVSyncInterval 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Int' peekIntConv*} -> `()'  #}

{- function isActive -}
{#fun ogre_rw_isActive as isActive 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function isClosed -}
{#fun ogre_rw_isClosed as isClosed 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function isPrimary -}
{#fun ogre_rw_isPrimary as isPrimary 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function isFullScreen -}
{#fun ogre_rw_isFullScreen as isFullScreen 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function getMetrics -}
{#fun ogre_rw_getMetrics as getMetrics 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Int' peekIntConv*,
 alloca- `Int' peekIntConv*,
 alloca- `Int' peekIntConv*,
 alloca- `Int' peekIntConv*,
 alloca- `Int' peekIntConv*} -> `()'  #}

{- function isDeactivatedOnFocusChange -}
{#fun ogre_rw_isDeactivatedOnFocusChange as isDeactivatedOnFocusChange 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function setDeactivateOnFocusChange -}
{#fun ogre_rw_setDeactivateOnFocusChange as setDeactivateOnFocusChange 
{ withHG3DClass* `HG3DClass' ,
 fromBool `Bool' } -> `()'  #}

