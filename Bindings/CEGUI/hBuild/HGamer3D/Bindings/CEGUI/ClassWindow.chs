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


-- ClassWindow.chs

-- 

module HGamer3D.Bindings.CEGUI.ClassWindow where

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
{# import HGamer3D.Bindings.CEGUI.EnumVerticalAlignment #}
{# import HGamer3D.Bindings.CEGUI.EnumHorizontalAlignment #}
{# import HGamer3D.Bindings.CEGUI.EnumWindowUpdateMode #}

#include "ClassWindow.h"
{- function Window -}
{#fun cegui_wnd_construct as new 
{ withCString* `String' ,
 withCString* `String' ,
 alloca- `HG3DClass' peek*} -> `()'  #}

{- function ~Window -}
{#fun cegui_wnd_destruct as delete 
{ withHG3DClass* `HG3DClass' } -> `()'  #}

{- function getType -}
{#fun cegui_wnd_getType as getType 
{ withHG3DClass* `HG3DClass' ,
 alloc64k- `String' peekCString*} -> `()'  #}

{- function getName -}
{#fun cegui_wnd_getName as getName 
{ withHG3DClass* `HG3DClass' ,
 alloc64k- `String' peekCString*} -> `()'  #}

{- function isDestroyedByParent -}
{#fun cegui_wnd_isDestroyedByParent as isDestroyedByParent 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function isAlwaysOnTop -}
{#fun cegui_wnd_isAlwaysOnTop as isAlwaysOnTop 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function isDisabled -}
{#fun cegui_wnd_isDisabled as isDisabled 
{ withHG3DClass* `HG3DClass' ,
 fromBool `Bool' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function isVisible -}
{#fun cegui_wnd_isVisible as isVisible 
{ withHG3DClass* `HG3DClass' ,
 fromBool `Bool' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function isActive -}
{#fun cegui_wnd_isActive as isActive 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function isClippedByParent -}
{#fun cegui_wnd_isClippedByParent as isClippedByParent 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function getID -}
{#fun cegui_wnd_getID as getID 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Int' peekIntConv*} -> `()'  #}

{- function getChildCount -}
{#fun cegui_wnd_getChildCount as getChildCount 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Int' peekIntConv*} -> `()'  #}

{- function isChild -}
{#fun cegui_wnd_isChild as isChild 
{ withHG3DClass* `HG3DClass' ,
 withCString* `String' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function isChild2 -}
{#fun cegui_wnd_isChild2 as isChild2 
{ withHG3DClass* `HG3DClass' ,
 fromIntegral `Int' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function isChildRecursive -}
{#fun cegui_wnd_isChildRecursive as isChildRecursive 
{ withHG3DClass* `HG3DClass' ,
 fromIntegral `Int' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function isChild3 -}
{#fun cegui_wnd_isChild3 as isChild3 
{ withHG3DClass* `HG3DClass' ,
 withHG3DClass* `HG3DClass' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function getChild -}
{#fun cegui_wnd_getChild as getChild 
{ withHG3DClass* `HG3DClass' ,
 withCString* `String' ,
 alloca- `HG3DClass' peek*} -> `()'  #}

{- function getChild2 -}
{#fun cegui_wnd_getChild2 as getChild2 
{ withHG3DClass* `HG3DClass' ,
 fromIntegral `Int' ,
 alloca- `HG3DClass' peek*} -> `()'  #}

{- function getChildRecursive -}
{#fun cegui_wnd_getChildRecursive as getChildRecursive 
{ withHG3DClass* `HG3DClass' ,
 withCString* `String' ,
 alloca- `HG3DClass' peek*} -> `()'  #}

{- function getChildRecursive2 -}
{#fun cegui_wnd_getChildRecursive2 as getChildRecursive2 
{ withHG3DClass* `HG3DClass' ,
 fromIntegral `Int' ,
 alloca- `HG3DClass' peek*} -> `()'  #}

{- function getChildAtIdx -}
{#fun cegui_wnd_getChildAtIdx as getChildAtIdx 
{ withHG3DClass* `HG3DClass' ,
 fromIntegral `Int' ,
 alloca- `HG3DClass' peek*} -> `()'  #}

{- function getActiveChild -}
{#fun cegui_wnd_getActiveChild as getActiveChild 
{ withHG3DClass* `HG3DClass' ,
 alloca- `HG3DClass' peek*} -> `()'  #}

{- function getActiveChild2 -}
{#fun cegui_wnd_getActiveChild2 as getActiveChild2 
{ withHG3DClass* `HG3DClass' ,
 alloca- `HG3DClass' peek*} -> `()'  #}

{- function isAncestor -}
{#fun cegui_wnd_isAncestor as isAncestor 
{ withHG3DClass* `HG3DClass' ,
 withCString* `String' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function isAncestor2 -}
{#fun cegui_wnd_isAncestor2 as isAncestor2 
{ withHG3DClass* `HG3DClass' ,
 fromIntegral `Int' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function isAncestor3 -}
{#fun cegui_wnd_isAncestor3 as isAncestor3 
{ withHG3DClass* `HG3DClass' ,
 withHG3DClass* `HG3DClass' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function getFont -}
{#fun cegui_wnd_getFont as getFont 
{ withHG3DClass* `HG3DClass' ,
 fromBool `Bool' ,
 alloca- `HG3DClass' peek*} -> `()'  #}

{- function getText -}
{#fun cegui_wnd_getText as getText 
{ withHG3DClass* `HG3DClass' ,
 alloc64k- `String' peekCString*} -> `()'  #}

{- function getTextVisual -}
{#fun cegui_wnd_getTextVisual as getTextVisual 
{ withHG3DClass* `HG3DClass' ,
 alloc64k- `String' peekCString*} -> `()'  #}

{- function inheritsAlpha -}
{#fun cegui_wnd_inheritsAlpha as inheritsAlpha 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function getAlpha -}
{#fun cegui_wnd_getAlpha as getAlpha 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Float' peekFloatConv*} -> `()'  #}

{- function getEffectiveAlpha -}
{#fun cegui_wnd_getEffectiveAlpha as getEffectiveAlpha 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Float' peekFloatConv*} -> `()'  #}

{- function isCapturedByThis -}
{#fun cegui_wnd_isCapturedByThis as isCapturedByThis 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function isCapturedByAncestor -}
{#fun cegui_wnd_isCapturedByAncestor as isCapturedByAncestor 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function isCapturedByChild -}
{#fun cegui_wnd_isCapturedByChild as isCapturedByChild 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function getParent -}
{#fun cegui_wnd_getParent as getParent 
{ withHG3DClass* `HG3DClass' ,
 alloca- `HG3DClass' peek*} -> `()'  #}

{- function restoresOldCapture -}
{#fun cegui_wnd_restoresOldCapture as restoresOldCapture 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function isZOrderingEnabled -}
{#fun cegui_wnd_isZOrderingEnabled as isZOrderingEnabled 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function wantsMultiClickEvents -}
{#fun cegui_wnd_wantsMultiClickEvents as wantsMultiClickEvents 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function isMouseAutoRepeatEnabled -}
{#fun cegui_wnd_isMouseAutoRepeatEnabled as isMouseAutoRepeatEnabled 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function getAutoRepeatDelay -}
{#fun cegui_wnd_getAutoRepeatDelay as getAutoRepeatDelay 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Float' peekFloatConv*} -> `()'  #}

{- function getAutoRepeatRate -}
{#fun cegui_wnd_getAutoRepeatRate as getAutoRepeatRate 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Float' peekFloatConv*} -> `()'  #}

{- function distributesCapturedInputs -}
{#fun cegui_wnd_distributesCapturedInputs as distributesCapturedInputs 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function isUsingDefaultTooltip -}
{#fun cegui_wnd_isUsingDefaultTooltip as isUsingDefaultTooltip 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function getTooltip -}
{#fun cegui_wnd_getTooltip as getTooltip 
{ withHG3DClass* `HG3DClass' ,
 alloca- `HG3DClass' peek*} -> `()'  #}

{- function getTooltipType -}
{#fun cegui_wnd_getTooltipType as getTooltipType 
{ withHG3DClass* `HG3DClass' ,
 alloc64k- `String' peekCString*} -> `()'  #}

{- function getTooltipText -}
{#fun cegui_wnd_getTooltipText as getTooltipText 
{ withHG3DClass* `HG3DClass' ,
 alloc64k- `String' peekCString*} -> `()'  #}

{- function inheritsTooltipText -}
{#fun cegui_wnd_inheritsTooltipText as inheritsTooltipText 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function isRiseOnClickEnabled -}
{#fun cegui_wnd_isRiseOnClickEnabled as isRiseOnClickEnabled 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function testClassName -}
{#fun cegui_wnd_testClassName as testClassName 
{ withHG3DClass* `HG3DClass' ,
 withCString* `String' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function getVerticalAlignment -}
{#fun cegui_wnd_getVerticalAlignment as getVerticalAlignment 
{ withHG3DClass* `HG3DClass' ,
 alloca- `EnumVerticalAlignment' peekEnumUtil*} -> `()'  #}

{- function getHorizontalAlignment -}
{#fun cegui_wnd_getHorizontalAlignment as getHorizontalAlignment 
{ withHG3DClass* `HG3DClass' ,
 alloca- `EnumHorizontalAlignment' peekEnumUtil*} -> `()'  #}

{- function getLookNFeel -}
{#fun cegui_wnd_getLookNFeel as getLookNFeel 
{ withHG3DClass* `HG3DClass' ,
 alloc64k- `String' peekCString*} -> `()'  #}

{- function getModalState -}
{#fun cegui_wnd_getModalState as getModalState 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function getUserString -}
{#fun cegui_wnd_getUserString as getUserString 
{ withHG3DClass* `HG3DClass' ,
 withCString* `String' ,
 alloc64k- `String' peekCString*} -> `()'  #}

{- function isUserStringDefined -}
{#fun cegui_wnd_isUserStringDefined as isUserStringDefined 
{ withHG3DClass* `HG3DClass' ,
 withCString* `String' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function getActiveSibling -}
{#fun cegui_wnd_getActiveSibling as getActiveSibling 
{ withHG3DClass* `HG3DClass' ,
 alloca- `HG3DClass' peek*} -> `()'  #}

{- function getParentPixelWidth -}
{#fun cegui_wnd_getParentPixelWidth as getParentPixelWidth 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Float' peekFloatConv*} -> `()'  #}

{- function getParentPixelHeight -}
{#fun cegui_wnd_getParentPixelHeight as getParentPixelHeight 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Float' peekFloatConv*} -> `()'  #}

{- function isMousePassThroughEnabled -}
{#fun cegui_wnd_isMousePassThroughEnabled as isMousePassThroughEnabled 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function isAutoWindow -}
{#fun cegui_wnd_isAutoWindow as isAutoWindow 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function isWritingXMLAllowed -}
{#fun cegui_wnd_isWritingXMLAllowed as isWritingXMLAllowed 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function isDragDropTarget -}
{#fun cegui_wnd_isDragDropTarget as isDragDropTarget 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function isUsingAutoRenderingSurface -}
{#fun cegui_wnd_isUsingAutoRenderingSurface as isUsingAutoRenderingSurface 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function getRootWindow -}
{#fun cegui_wnd_getRootWindow as getRootWindow 
{ withHG3DClass* `HG3DClass' ,
 alloca- `HG3DClass' peek*} -> `()'  #}

{- function getRootWindow2 -}
{#fun cegui_wnd_getRootWindow2 as getRootWindow2 
{ withHG3DClass* `HG3DClass' ,
 alloca- `HG3DClass' peek*} -> `()'  #}

{- function isNonClientWindow -}
{#fun cegui_wnd_isNonClientWindow as isNonClientWindow 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function rename -}
{#fun cegui_wnd_rename as rename 
{ withHG3DClass* `HG3DClass' ,
 withCString* `String' } -> `()'  #}

{- function initialiseComponents -}
{#fun cegui_wnd_initialiseComponents as initialiseComponents 
{ withHG3DClass* `HG3DClass' } -> `()'  #}

{- function setDestroyedByParent -}
{#fun cegui_wnd_setDestroyedByParent as setDestroyedByParent 
{ withHG3DClass* `HG3DClass' ,
 fromBool `Bool' } -> `()'  #}

{- function setAlwaysOnTop -}
{#fun cegui_wnd_setAlwaysOnTop as setAlwaysOnTop 
{ withHG3DClass* `HG3DClass' ,
 fromBool `Bool' } -> `()'  #}

{- function setEnabled -}
{#fun cegui_wnd_setEnabled as setEnabled 
{ withHG3DClass* `HG3DClass' ,
 fromBool `Bool' } -> `()'  #}

{- function enable -}
{#fun cegui_wnd_enable as enable 
{ withHG3DClass* `HG3DClass' } -> `()'  #}

{- function disable -}
{#fun cegui_wnd_disable as disable 
{ withHG3DClass* `HG3DClass' } -> `()'  #}

{- function setVisible -}
{#fun cegui_wnd_setVisible as setVisible 
{ withHG3DClass* `HG3DClass' ,
 fromBool `Bool' } -> `()'  #}

{- function show -}
{#fun cegui_wnd_show as show 
{ withHG3DClass* `HG3DClass' } -> `()'  #}

{- function hide -}
{#fun cegui_wnd_hide as hide 
{ withHG3DClass* `HG3DClass' } -> `()'  #}

{- function activate -}
{#fun cegui_wnd_activate as activate 
{ withHG3DClass* `HG3DClass' } -> `()'  #}

{- function deactivate -}
{#fun cegui_wnd_deactivate as deactivate 
{ withHG3DClass* `HG3DClass' } -> `()'  #}

{- function setClippedByParent -}
{#fun cegui_wnd_setClippedByParent as setClippedByParent 
{ withHG3DClass* `HG3DClass' ,
 fromBool `Bool' } -> `()'  #}

{- function setID -}
{#fun cegui_wnd_setID as setID 
{ withHG3DClass* `HG3DClass' ,
 fromIntegral `Int' } -> `()'  #}

{- function setText -}
{#fun cegui_wnd_setText as setText 
{ withHG3DClass* `HG3DClass' ,
 withCString* `String' } -> `()'  #}

{- function appendText -}
{#fun cegui_wnd_appendText as appendText 
{ withHG3DClass* `HG3DClass' ,
 withCString* `String' } -> `()'  #}

{- function setFont -}
{#fun cegui_wnd_setFont as setFont 
{ withHG3DClass* `HG3DClass' ,
 withHG3DClass* `HG3DClass' } -> `()'  #}

{- function setFont2 -}
{#fun cegui_wnd_setFont2 as setFont2 
{ withHG3DClass* `HG3DClass' ,
 withCString* `String' } -> `()'  #}

{- function addChildWindow -}
{#fun cegui_wnd_addChildWindow as addChildWindow 
{ withHG3DClass* `HG3DClass' ,
 withCString* `String' } -> `()'  #}

{- function addChildWindow2 -}
{#fun cegui_wnd_addChildWindow2 as addChildWindow2 
{ withHG3DClass* `HG3DClass' ,
 withHG3DClass* `HG3DClass' } -> `()'  #}

{- function removeChildWindow -}
{#fun cegui_wnd_removeChildWindow as removeChildWindow 
{ withHG3DClass* `HG3DClass' ,
 withCString* `String' } -> `()'  #}

{- function removeChildWindow2 -}
{#fun cegui_wnd_removeChildWindow2 as removeChildWindow2 
{ withHG3DClass* `HG3DClass' ,
 withHG3DClass* `HG3DClass' } -> `()'  #}

{- function removeChildWindow3 -}
{#fun cegui_wnd_removeChildWindow3 as removeChildWindow3 
{ withHG3DClass* `HG3DClass' ,
 fromIntegral `Int' } -> `()'  #}

{- function moveToFront -}
{#fun cegui_wnd_moveToFront as moveToFront 
{ withHG3DClass* `HG3DClass' } -> `()'  #}

{- function moveToBack -}
{#fun cegui_wnd_moveToBack as moveToBack 
{ withHG3DClass* `HG3DClass' } -> `()'  #}

{- function captureInput -}
{#fun cegui_wnd_captureInput as captureInput 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function releaseInput -}
{#fun cegui_wnd_releaseInput as releaseInput 
{ withHG3DClass* `HG3DClass' } -> `()'  #}

{- function setRestoreCapture -}
{#fun cegui_wnd_setRestoreCapture as setRestoreCapture 
{ withHG3DClass* `HG3DClass' ,
 fromBool `Bool' } -> `()'  #}

{- function setAlpha -}
{#fun cegui_wnd_setAlpha as setAlpha 
{ withHG3DClass* `HG3DClass' ,
 realToFrac `Float' } -> `()'  #}

{- function setInheritsAlpha -}
{#fun cegui_wnd_setInheritsAlpha as setInheritsAlpha 
{ withHG3DClass* `HG3DClass' ,
 fromBool `Bool' } -> `()'  #}

{- function invalidate -}
{#fun cegui_wnd_invalidate as invalidate 
{ withHG3DClass* `HG3DClass' } -> `()'  #}

{- function invalidate2 -}
{#fun cegui_wnd_invalidate2 as invalidate2 
{ withHG3DClass* `HG3DClass' ,
 fromBool `Bool' } -> `()'  #}

{- function setMouseCursor3 -}
{#fun cegui_wnd_setMouseCursor3 as setMouseCursor3 
{ withHG3DClass* `HG3DClass' ,
 withCString* `String' ,
 withCString* `String' } -> `()'  #}

{- function setZOrderingEnabled -}
{#fun cegui_wnd_setZOrderingEnabled as setZOrderingEnabled 
{ withHG3DClass* `HG3DClass' ,
 fromBool `Bool' } -> `()'  #}

{- function setWantsMultiClickEvents -}
{#fun cegui_wnd_setWantsMultiClickEvents as setWantsMultiClickEvents 
{ withHG3DClass* `HG3DClass' ,
 fromBool `Bool' } -> `()'  #}

{- function setMouseAutoRepeatEnabled -}
{#fun cegui_wnd_setMouseAutoRepeatEnabled as setMouseAutoRepeatEnabled 
{ withHG3DClass* `HG3DClass' ,
 fromBool `Bool' } -> `()'  #}

{- function setAutoRepeatDelay -}
{#fun cegui_wnd_setAutoRepeatDelay as setAutoRepeatDelay 
{ withHG3DClass* `HG3DClass' ,
 realToFrac `Float' } -> `()'  #}

{- function setAutoRepeatRate -}
{#fun cegui_wnd_setAutoRepeatRate as setAutoRepeatRate 
{ withHG3DClass* `HG3DClass' ,
 realToFrac `Float' } -> `()'  #}

{- function setDistributesCapturedInputs -}
{#fun cegui_wnd_setDistributesCapturedInputs as setDistributesCapturedInputs 
{ withHG3DClass* `HG3DClass' ,
 fromBool `Bool' } -> `()'  #}

{- function notifyDragDropItemEnters -}
{#fun cegui_wnd_notifyDragDropItemEnters as notifyDragDropItemEnters 
{ withHG3DClass* `HG3DClass' ,
 withHG3DClass* `HG3DClass' } -> `()'  #}

{- function notifyDragDropItemLeaves -}
{#fun cegui_wnd_notifyDragDropItemLeaves as notifyDragDropItemLeaves 
{ withHG3DClass* `HG3DClass' ,
 withHG3DClass* `HG3DClass' } -> `()'  #}

{- function notifyDragDropItemDropped -}
{#fun cegui_wnd_notifyDragDropItemDropped as notifyDragDropItemDropped 
{ withHG3DClass* `HG3DClass' ,
 withHG3DClass* `HG3DClass' } -> `()'  #}

{- function destroy -}
{#fun cegui_wnd_destroy as destroy 
{ withHG3DClass* `HG3DClass' } -> `()'  #}

{- function setTooltip -}
{#fun cegui_wnd_setTooltip as setTooltip 
{ withHG3DClass* `HG3DClass' ,
 withHG3DClass* `HG3DClass' } -> `()'  #}

{- function setTooltipType -}
{#fun cegui_wnd_setTooltipType as setTooltipType 
{ withHG3DClass* `HG3DClass' ,
 withCString* `String' } -> `()'  #}

{- function setTooltipText -}
{#fun cegui_wnd_setTooltipText as setTooltipText 
{ withHG3DClass* `HG3DClass' ,
 withCString* `String' } -> `()'  #}

{- function setInheritsTooltipText -}
{#fun cegui_wnd_setInheritsTooltipText as setInheritsTooltipText 
{ withHG3DClass* `HG3DClass' ,
 fromBool `Bool' } -> `()'  #}

{- function setRiseOnClickEnabled -}
{#fun cegui_wnd_setRiseOnClickEnabled as setRiseOnClickEnabled 
{ withHG3DClass* `HG3DClass' ,
 fromBool `Bool' } -> `()'  #}

{- function setVerticalAlignment -}
{#fun cegui_wnd_setVerticalAlignment as setVerticalAlignment 
{ withHG3DClass* `HG3DClass' ,
 cIntFromEnum `EnumVerticalAlignment' } -> `()'  #}

{- function setHorizontalAlignment -}
{#fun cegui_wnd_setHorizontalAlignment as setHorizontalAlignment 
{ withHG3DClass* `HG3DClass' ,
 cIntFromEnum `EnumHorizontalAlignment' } -> `()'  #}

{- function setLookNFeel -}
{#fun cegui_wnd_setLookNFeel as setLookNFeel 
{ withHG3DClass* `HG3DClass' ,
 withCString* `String' } -> `()'  #}

{- function setModalState -}
{#fun cegui_wnd_setModalState as setModalState 
{ withHG3DClass* `HG3DClass' ,
 fromBool `Bool' } -> `()'  #}

{- function performChildWindowLayout -}
{#fun cegui_wnd_performChildWindowLayout as performChildWindowLayout 
{ withHG3DClass* `HG3DClass' } -> `()'  #}

{- function setUserString -}
{#fun cegui_wnd_setUserString as setUserString 
{ withHG3DClass* `HG3DClass' ,
 withCString* `String' ,
 withCString* `String' } -> `()'  #}

{- function setArea -}
{#fun cegui_wnd_setArea as setArea 
{ withHG3DClass* `HG3DClass' ,
 withHG3DClass* `HG3DClass' ,
 withHG3DClass* `HG3DClass' ,
 withHG3DClass* `HG3DClass' ,
 withHG3DClass* `HG3DClass' } -> `()'  #}

{- function setArea2 -}
{#fun cegui_wnd_setArea2 as setArea2 
{ withHG3DClass* `HG3DClass' ,
 withHG3DClass* `HG3DClass' ,
 withHG3DClass* `HG3DClass' } -> `()'  #}

{- function setPosition -}
{#fun cegui_wnd_setPosition as setPosition 
{ withHG3DClass* `HG3DClass' ,
 withHG3DClass* `HG3DClass' } -> `()'  #}

{- function setXPosition -}
{#fun cegui_wnd_setXPosition as setXPosition 
{ withHG3DClass* `HG3DClass' ,
 withHG3DClass* `HG3DClass' } -> `()'  #}

{- function setYPosition -}
{#fun cegui_wnd_setYPosition as setYPosition 
{ withHG3DClass* `HG3DClass' ,
 withHG3DClass* `HG3DClass' } -> `()'  #}

{- function setSize -}
{#fun cegui_wnd_setSize as setSize 
{ withHG3DClass* `HG3DClass' ,
 withHG3DClass* `HG3DClass' } -> `()'  #}

{- function setWidth -}
{#fun cegui_wnd_setWidth as setWidth 
{ withHG3DClass* `HG3DClass' ,
 withHG3DClass* `HG3DClass' } -> `()'  #}

{- function setHeight -}
{#fun cegui_wnd_setHeight as setHeight 
{ withHG3DClass* `HG3DClass' ,
 withHG3DClass* `HG3DClass' } -> `()'  #}

{- function setMaxSize -}
{#fun cegui_wnd_setMaxSize as setMaxSize 
{ withHG3DClass* `HG3DClass' ,
 withHG3DClass* `HG3DClass' } -> `()'  #}

{- function setMinSize -}
{#fun cegui_wnd_setMinSize as setMinSize 
{ withHG3DClass* `HG3DClass' ,
 withHG3DClass* `HG3DClass' } -> `()'  #}

{- function getPosition -}
{#fun cegui_wnd_getPosition as getPosition 
{ withHG3DClass* `HG3DClass' ,
 alloca- `HG3DClass' peek*} -> `()'  #}

{- function getXPosition -}
{#fun cegui_wnd_getXPosition as getXPosition 
{ withHG3DClass* `HG3DClass' ,
 alloca- `HG3DClass' peek*} -> `()'  #}

{- function getYPosition -}
{#fun cegui_wnd_getYPosition as getYPosition 
{ withHG3DClass* `HG3DClass' ,
 alloca- `HG3DClass' peek*} -> `()'  #}

{- function getMaxSize -}
{#fun cegui_wnd_getMaxSize as getMaxSize 
{ withHG3DClass* `HG3DClass' ,
 alloca- `HG3DClass' peek*} -> `()'  #}

{- function getMinSize -}
{#fun cegui_wnd_getMinSize as getMinSize 
{ withHG3DClass* `HG3DClass' ,
 alloca- `HG3DClass' peek*} -> `()'  #}

{- function render -}
{#fun cegui_wnd_render as render 
{ withHG3DClass* `HG3DClass' } -> `()'  #}

{- function update -}
{#fun cegui_wnd_update as update 
{ withHG3DClass* `HG3DClass' ,
 realToFrac `Float' } -> `()'  #}

{- function beginInitialisation -}
{#fun cegui_wnd_beginInitialisation as beginInitialisation 
{ withHG3DClass* `HG3DClass' } -> `()'  #}

{- function endInitialisation -}
{#fun cegui_wnd_endInitialisation as endInitialisation 
{ withHG3DClass* `HG3DClass' } -> `()'  #}

{- function setMousePassThroughEnabled -}
{#fun cegui_wnd_setMousePassThroughEnabled as setMousePassThroughEnabled 
{ withHG3DClass* `HG3DClass' ,
 fromBool `Bool' } -> `()'  #}

{- function setWindowRenderer -}
{#fun cegui_wnd_setWindowRenderer as setWindowRenderer 
{ withHG3DClass* `HG3DClass' ,
 withCString* `String' } -> `()'  #}

{- function getWindowRendererName -}
{#fun cegui_wnd_getWindowRendererName as getWindowRendererName 
{ withHG3DClass* `HG3DClass' ,
 alloc64k- `String' peekCString*} -> `()'  #}

{- function setWritingXMLAllowed -}
{#fun cegui_wnd_setWritingXMLAllowed as setWritingXMLAllowed 
{ withHG3DClass* `HG3DClass' ,
 fromBool `Bool' } -> `()'  #}

{- function notifyScreenAreaChanged -}
{#fun cegui_wnd_notifyScreenAreaChanged as notifyScreenAreaChanged 
{ withHG3DClass* `HG3DClass' ,
 fromBool `Bool' } -> `()'  #}

{- function setFalagardType -}
{#fun cegui_wnd_setFalagardType as setFalagardType 
{ withHG3DClass* `HG3DClass' ,
 withCString* `String' ,
 withCString* `String' } -> `()'  #}

{- function setDragDropTarget -}
{#fun cegui_wnd_setDragDropTarget as setDragDropTarget 
{ withHG3DClass* `HG3DClass' ,
 fromBool `Bool' } -> `()'  #}

{- function invalidateRenderingSurface -}
{#fun cegui_wnd_invalidateRenderingSurface as invalidateRenderingSurface 
{ withHG3DClass* `HG3DClass' } -> `()'  #}

{- function setUsingAutoRenderingSurface -}
{#fun cegui_wnd_setUsingAutoRenderingSurface as setUsingAutoRenderingSurface 
{ withHG3DClass* `HG3DClass' ,
 fromBool `Bool' } -> `()'  #}

{- function setNonClientWindow -}
{#fun cegui_wnd_setNonClientWindow as setNonClientWindow 
{ withHG3DClass* `HG3DClass' ,
 fromBool `Bool' } -> `()'  #}

{- function isTextParsingEnabled -}
{#fun cegui_wnd_isTextParsingEnabled as isTextParsingEnabled 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function setTextParsingEnabled -}
{#fun cegui_wnd_setTextParsingEnabled as setTextParsingEnabled 
{ withHG3DClass* `HG3DClass' ,
 fromBool `Bool' } -> `()'  #}

{- function banPropertyFromXML -}
{#fun cegui_wnd_banPropertyFromXML as banPropertyFromXML 
{ withHG3DClass* `HG3DClass' ,
 withCString* `String' } -> `()'  #}

{- function unbanPropertyFromXML -}
{#fun cegui_wnd_unbanPropertyFromXML as unbanPropertyFromXML 
{ withHG3DClass* `HG3DClass' ,
 withCString* `String' } -> `()'  #}

{- function isPropertyBannedFromXML -}
{#fun cegui_wnd_isPropertyBannedFromXML as isPropertyBannedFromXML 
{ withHG3DClass* `HG3DClass' ,
 withCString* `String' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function setUpdateMode -}
{#fun cegui_wnd_setUpdateMode as setUpdateMode 
{ withHG3DClass* `HG3DClass' ,
 cIntFromEnum `EnumWindowUpdateMode' } -> `()'  #}

{- function getUpdateMode -}
{#fun cegui_wnd_getUpdateMode as getUpdateMode 
{ withHG3DClass* `HG3DClass' ,
 alloca- `EnumWindowUpdateMode' peekEnumUtil*} -> `()'  #}

{- function setMouseInputPropagationEnabled -}
{#fun cegui_wnd_setMouseInputPropagationEnabled as setMouseInputPropagationEnabled 
{ withHG3DClass* `HG3DClass' ,
 fromBool `Bool' } -> `()'  #}

{- function isMouseInputPropagationEnabled -}
{#fun cegui_wnd_isMouseInputPropagationEnabled as isMouseInputPropagationEnabled 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function clone -}
{#fun cegui_wnd_clone as clone 
{ withHG3DClass* `HG3DClass' ,
 withCString* `String' ,
 fromBool `Bool' ,
 alloca- `HG3DClass' peek*} -> `()'  #}

{- function clonePropertiesTo -}
{#fun cegui_wnd_clonePropertiesTo as clonePropertiesTo 
{ withHG3DClass* `HG3DClass' ,
 withHG3DClass* `HG3DClass' } -> `()'  #}

{- function cloneChildWidgetsTo -}
{#fun cegui_wnd_cloneChildWidgetsTo as cloneChildWidgetsTo 
{ withHG3DClass* `HG3DClass' ,
 withHG3DClass* `HG3DClass' } -> `()'  #}

{- function getZIndex -}
{#fun cegui_wnd_getZIndex as getZIndex 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Int' peekIntConv*} -> `()'  #}

{- function isInFront -}
{#fun cegui_wnd_isInFront as isInFront 
{ withHG3DClass* `HG3DClass' ,
 withHG3DClass* `HG3DClass' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function isBehind -}
{#fun cegui_wnd_isBehind as isBehind 
{ withHG3DClass* `HG3DClass' ,
 withHG3DClass* `HG3DClass' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function getCaptureWindow -}
{#fun cegui_wnd_getCaptureWindow as getCaptureWindow 
{ alloca- `HG3DClass' peek*} -> `()'  #}

