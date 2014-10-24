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


-- ClassSystem.chs

-- 

module HGamer3D.Bindings.CEGUI.ClassSystem where

import Foreign
import Foreign.Ptr
import Foreign.C

import HGamer3D.Data.HG3DClass
import HGamer3D.Data.Vector
import HGamer3D.Data.Colour
import HGamer3D.Data.Angle

{# import HGamer3D.Bindings.CEGUI.Utils #}
{# import HGamer3D.Bindings.CEGUI.ClassPtr #}
{# import HGamer3D.Bindings.CEGUI.StructHG3DClass #}
{# import HGamer3D.Bindings.CEGUI.EnumMouseButton #}

#include "ClassSystem.h"
{- function create -}
{#fun cegui_sstm_create as create 
{ withHG3DClass* `HG3DClass' ,
 withHG3DClass* `HG3DClass' ,
 withHG3DClass* `HG3DClass' ,
 withHG3DClass* `HG3DClass' ,
 withHG3DClass* `HG3DClass' ,
 withCString* `String' ,
 withCString* `String' ,
 alloca- `HG3DClass' peek*} -> `()'  #}

{- function destroy -}
{#fun cegui_sstm_destroy as destroy 
{ } -> `()'  #}

{- function getSingleton -}
{#fun cegui_sstm_getSingleton as getSingleton 
{ alloca- `HG3DClass' peek*} -> `()'  #}

{- function getSingletonPtr -}
{#fun cegui_sstm_getSingletonPtr as getSingletonPtr 
{ alloca- `HG3DClass' peek*} -> `()'  #}

{- function setDefaultXMLParserName -}
{#fun cegui_sstm_setDefaultXMLParserName as setDefaultXMLParserName 
{ withCString* `String' } -> `()'  #}

{- function getDefaultXMLParserName -}
{#fun cegui_sstm_getDefaultXMLParserName as getDefaultXMLParserName 
{ alloc64k- `String' peekCString*} -> `()'  #}

{- function setDefaultImageCodecName -}
{#fun cegui_sstm_setDefaultImageCodecName as setDefaultImageCodecName 
{ withCString* `String' } -> `()'  #}

{- function getDefaultImageCodecName -}
{#fun cegui_sstm_getDefaultImageCodecName as getDefaultImageCodecName 
{ alloc64k- `String' peekCString*} -> `()'  #}

{- function getRenderer -}
{#fun cegui_sstm_getRenderer as getRenderer 
{ withHG3DClass* `HG3DClass' ,
 alloca- `HG3DClass' peek*} -> `()'  #}

{- function setDefaultFont -}
{#fun cegui_sstm_setDefaultFont as setDefaultFont 
{ withHG3DClass* `HG3DClass' ,
 withCString* `String' } -> `()'  #}

{- function setDefaultFont2 -}
{#fun cegui_sstm_setDefaultFont2 as setDefaultFont2 
{ withHG3DClass* `HG3DClass' ,
 withHG3DClass* `HG3DClass' } -> `()'  #}

{- function getDefaultFont -}
{#fun cegui_sstm_getDefaultFont as getDefaultFont 
{ withHG3DClass* `HG3DClass' ,
 alloca- `HG3DClass' peek*} -> `()'  #}

{- function signalRedraw -}
{#fun cegui_sstm_signalRedraw as signalRedraw 
{ withHG3DClass* `HG3DClass' } -> `()'  #}

{- function isRedrawRequested -}
{#fun cegui_sstm_isRedrawRequested as isRedrawRequested 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function renderGUI -}
{#fun cegui_sstm_renderGUI as renderGUI 
{ withHG3DClass* `HG3DClass' } -> `()'  #}

{- function setGUISheet -}
{#fun cegui_sstm_setGUISheet as setGUISheet 
{ withHG3DClass* `HG3DClass' ,
 withHG3DClass* `HG3DClass' ,
 alloca- `HG3DClass' peek*} -> `()'  #}

{- function getGUISheet -}
{#fun cegui_sstm_getGUISheet as getGUISheet 
{ withHG3DClass* `HG3DClass' ,
 alloca- `HG3DClass' peek*} -> `()'  #}

{- function getSingleClickTimeout -}
{#fun cegui_sstm_getSingleClickTimeout as getSingleClickTimeout 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Double' peekFloatConv*} -> `()'  #}

{- function getMultiClickTimeout -}
{#fun cegui_sstm_getMultiClickTimeout as getMultiClickTimeout 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Double' peekFloatConv*} -> `()'  #}

{- function setSingleClickTimeout -}
{#fun cegui_sstm_setSingleClickTimeout as setSingleClickTimeout 
{ withHG3DClass* `HG3DClass' ,
 realToFrac `Double' } -> `()'  #}

{- function setMultiClickTimeout -}
{#fun cegui_sstm_setMultiClickTimeout as setMultiClickTimeout 
{ withHG3DClass* `HG3DClass' ,
 realToFrac `Double' } -> `()'  #}

{- function isMouseClickEventGenerationEnabled -}
{#fun cegui_sstm_isMouseClickEventGenerationEnabled as isMouseClickEventGenerationEnabled 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function setMouseClickEventGenerationEnabled -}
{#fun cegui_sstm_setMouseClickEventGenerationEnabled as setMouseClickEventGenerationEnabled 
{ withHG3DClass* `HG3DClass' ,
 fromBool `Bool' } -> `()'  #}

{- function setDefaultMouseCursor3 -}
{#fun cegui_sstm_setDefaultMouseCursor3 as setDefaultMouseCursor3 
{ withHG3DClass* `HG3DClass' ,
 withCString* `String' ,
 withCString* `String' } -> `()'  #}

{- function getWindowContainingMouse -}
{#fun cegui_sstm_getWindowContainingMouse as getWindowContainingMouse 
{ withHG3DClass* `HG3DClass' ,
 alloca- `HG3DClass' peek*} -> `()'  #}

{- function getScriptingModule -}
{#fun cegui_sstm_getScriptingModule as getScriptingModule 
{ withHG3DClass* `HG3DClass' ,
 alloca- `HG3DClass' peek*} -> `()'  #}

{- function setScriptingModule -}
{#fun cegui_sstm_setScriptingModule as setScriptingModule 
{ withHG3DClass* `HG3DClass' ,
 withHG3DClass* `HG3DClass' } -> `()'  #}

{- function getResourceProvider -}
{#fun cegui_sstm_getResourceProvider as getResourceProvider 
{ withHG3DClass* `HG3DClass' ,
 alloca- `HG3DClass' peek*} -> `()'  #}

{- function executeScriptFile -}
{#fun cegui_sstm_executeScriptFile as executeScriptFile 
{ withHG3DClass* `HG3DClass' ,
 withCString* `String' ,
 withCString* `String' } -> `()'  #}

{- function executeScriptGlobal -}
{#fun cegui_sstm_executeScriptGlobal as executeScriptGlobal 
{ withHG3DClass* `HG3DClass' ,
 withCString* `String' ,
 alloca- `Int' peekIntConv*} -> `()'  #}

{- function executeScriptString -}
{#fun cegui_sstm_executeScriptString as executeScriptString 
{ withHG3DClass* `HG3DClass' ,
 withCString* `String' } -> `()'  #}

{- function getMouseMoveScaling -}
{#fun cegui_sstm_getMouseMoveScaling as getMouseMoveScaling 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Float' peekFloatConv*} -> `()'  #}

{- function setMouseMoveScaling -}
{#fun cegui_sstm_setMouseMoveScaling as setMouseMoveScaling 
{ withHG3DClass* `HG3DClass' ,
 realToFrac `Float' } -> `()'  #}

{- function notifyWindowDestroyed -}
{#fun cegui_sstm_notifyWindowDestroyed as notifyWindowDestroyed 
{ withHG3DClass* `HG3DClass' ,
 withHG3DClass* `HG3DClass' } -> `()'  #}

{- function getSystemKeys -}
{#fun cegui_sstm_getSystemKeys as getSystemKeys 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Int' peekIntConv*} -> `()'  #}

{- function setXMLParser -}
{#fun cegui_sstm_setXMLParser as setXMLParser 
{ withHG3DClass* `HG3DClass' ,
 withCString* `String' } -> `()'  #}

{- function setXMLParser2 -}
{#fun cegui_sstm_setXMLParser2 as setXMLParser2 
{ withHG3DClass* `HG3DClass' ,
 withHG3DClass* `HG3DClass' } -> `()'  #}

{- function getXMLParser -}
{#fun cegui_sstm_getXMLParser as getXMLParser 
{ withHG3DClass* `HG3DClass' ,
 alloca- `HG3DClass' peek*} -> `()'  #}

{- function setDefaultTooltip -}
{#fun cegui_sstm_setDefaultTooltip as setDefaultTooltip 
{ withHG3DClass* `HG3DClass' ,
 withHG3DClass* `HG3DClass' } -> `()'  #}

{- function setDefaultTooltip2 -}
{#fun cegui_sstm_setDefaultTooltip2 as setDefaultTooltip2 
{ withHG3DClass* `HG3DClass' ,
 withCString* `String' } -> `()'  #}

{- function getDefaultTooltip -}
{#fun cegui_sstm_getDefaultTooltip as getDefaultTooltip 
{ withHG3DClass* `HG3DClass' ,
 alloca- `HG3DClass' peek*} -> `()'  #}

{- function setModalTarget -}
{#fun cegui_sstm_setModalTarget as setModalTarget 
{ withHG3DClass* `HG3DClass' ,
 withHG3DClass* `HG3DClass' } -> `()'  #}

{- function getModalTarget -}
{#fun cegui_sstm_getModalTarget as getModalTarget 
{ withHG3DClass* `HG3DClass' ,
 alloca- `HG3DClass' peek*} -> `()'  #}

{- function updateWindowContainingMouse -}
{#fun cegui_sstm_updateWindowContainingMouse as updateWindowContainingMouse 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function getImageCodec -}
{#fun cegui_sstm_getImageCodec as getImageCodec 
{ withHG3DClass* `HG3DClass' ,
 alloca- `HG3DClass' peek*} -> `()'  #}

{- function setImageCodec -}
{#fun cegui_sstm_setImageCodec as setImageCodec 
{ withHG3DClass* `HG3DClass' ,
 withCString* `String' } -> `()'  #}

{- function setImageCodec2 -}
{#fun cegui_sstm_setImageCodec2 as setImageCodec2 
{ withHG3DClass* `HG3DClass' ,
 withHG3DClass* `HG3DClass' } -> `()'  #}

{- function invalidateAllCachedRendering -}
{#fun cegui_sstm_invalidateAllCachedRendering as invalidateAllCachedRendering 
{ withHG3DClass* `HG3DClass' } -> `()'  #}

{- function injectMouseMove -}
{#fun cegui_sstm_injectMouseMove as injectMouseMove 
{ withHG3DClass* `HG3DClass' ,
 realToFrac `Float' ,
 realToFrac `Float' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function injectMouseLeaves -}
{#fun cegui_sstm_injectMouseLeaves as injectMouseLeaves 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function injectMouseButtonDown -}
{#fun cegui_sstm_injectMouseButtonDown as injectMouseButtonDown 
{ withHG3DClass* `HG3DClass' ,
 cIntFromEnum `EnumMouseButton' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function injectMouseButtonUp -}
{#fun cegui_sstm_injectMouseButtonUp as injectMouseButtonUp 
{ withHG3DClass* `HG3DClass' ,
 cIntFromEnum `EnumMouseButton' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function injectKeyDown -}
{#fun cegui_sstm_injectKeyDown as injectKeyDown 
{ withHG3DClass* `HG3DClass' ,
 fromIntegral `Int' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function injectKeyUp -}
{#fun cegui_sstm_injectKeyUp as injectKeyUp 
{ withHG3DClass* `HG3DClass' ,
 fromIntegral `Int' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function injectChar -}
{#fun cegui_sstm_injectChar as injectChar 
{ withHG3DClass* `HG3DClass' ,
 fromIntegral `Int' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function injectMouseWheelChange -}
{#fun cegui_sstm_injectMouseWheelChange as injectMouseWheelChange 
{ withHG3DClass* `HG3DClass' ,
 realToFrac `Float' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function injectMousePosition -}
{#fun cegui_sstm_injectMousePosition as injectMousePosition 
{ withHG3DClass* `HG3DClass' ,
 realToFrac `Float' ,
 realToFrac `Float' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function injectTimePulse -}
{#fun cegui_sstm_injectTimePulse as injectTimePulse 
{ withHG3DClass* `HG3DClass' ,
 realToFrac `Float' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function injectMouseButtonClick -}
{#fun cegui_sstm_injectMouseButtonClick as injectMouseButtonClick 
{ withHG3DClass* `HG3DClass' ,
 cIntFromEnum `EnumMouseButton' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function injectMouseButtonDoubleClick -}
{#fun cegui_sstm_injectMouseButtonDoubleClick as injectMouseButtonDoubleClick 
{ withHG3DClass* `HG3DClass' ,
 cIntFromEnum `EnumMouseButton' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function injectMouseButtonTripleClick -}
{#fun cegui_sstm_injectMouseButtonTripleClick as injectMouseButtonTripleClick 
{ withHG3DClass* `HG3DClass' ,
 cIntFromEnum `EnumMouseButton' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

