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


-- ClassEnetClient.chs

-- 

module HGamer3D.Bindings.Enet.ClassEnetClient where

import Foreign
import Foreign.Ptr
import Foreign.C

import HGamer3D.Data.HG3DClass
import HGamer3D.Data.Vector
import HGamer3D.Data.Colour
import HGamer3D.Data.Angle

{# import HGamer3D.Bindings.Enet.Utils #}
{# import HGamer3D.Bindings.Enet.ClassPtr #}
{# import HGamer3D.Bindings.Enet.StructHG3DClass #}

#include "ClassEnetClient.h"
{- function ~EnetClient -}
{#fun enet_clnt_destruct as delete 
{ withHG3DClass* `HG3DClass' } -> `()'  #}

{- function connect -}
{#fun enet_clnt_connect as connect 
{ withHG3DClass* `HG3DClass' ,
 withCString* `String' ,
 fromIntegral `Int' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function disconnect -}
{#fun enet_clnt_disconnect as disconnect 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function serve -}
{#fun enet_clnt_serve as serve 
{ withHG3DClass* `HG3DClass' ,
 fromIntegral `Int' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function getPacket -}
{#fun enet_clnt_getPacket as getPacket 
{ withHG3DClass* `HG3DClass' ,
 alloca- `HG3DClass' peek*} -> `()'  #}

{- function send -}
{#fun enet_clnt_send as send 
{ withHG3DClass* `HG3DClass' ,
 withCString* `String' ,
 fromIntegral `Int' } -> `()'  #}

