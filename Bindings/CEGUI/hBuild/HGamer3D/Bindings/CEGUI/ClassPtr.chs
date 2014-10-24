{-# LANGUAGE EmptyDataDecls #-}
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


-- ClassPtr.chs

-- Class Ptr Utilities

module HGamer3D.Bindings.CEGUI.ClassPtr where

import Foreign
import Foreign.Ptr
import Foreign.C

import HGamer3D.Data.HG3DClass
import HGamer3D.Data.Vector
import HGamer3D.Data.Colour
import HGamer3D.Data.Angle

{# import HGamer3D.Bindings.CEGUI.Utils #}

#include "ClassPtr.h"
{- class ClassCheckbox -}
{#pointer *ClassCheckbox as ClassCheckbox#}
{- class ClassCombobox -}
{#pointer *ClassCombobox as ClassCombobox#}
{- class ClassComboDropList -}
{#pointer *ClassComboDropList as ClassComboDropList#}
{- class ClassDefaultLogger -}
{#pointer *ClassDefaultLogger as ClassDefaultLogger#}
{- class ClassDefaultResourceProvider -}
{#pointer *ClassDefaultResourceProvider as ClassDefaultResourceProvider#}
{- class ClassDragContainer -}
{#pointer *ClassDragContainer as ClassDragContainer#}
{- class ClassEditbox -}
{#pointer *ClassEditbox as ClassEditbox#}
{- class ClassEventArgs -}
{#pointer *ClassEventArgs as ClassEventArgs#}
{- class ClassEventSet -}
{#pointer *ClassEventSet as ClassEventSet#}
{- class ClassFont -}
{#pointer *ClassFont as ClassFont#}
{- class ClassFontManager -}
{#pointer *ClassFontManager as ClassFontManager#}
{- class ClassFrameWindow -}
{#pointer *ClassFrameWindow as ClassFrameWindow#}
{- class ClassImageCodec -}
{#pointer *ClassImageCodec as ClassImageCodec#}
{- class ClassImageset -}
{#pointer *ClassImageset as ClassImageset#}
{- class ClassItemEntry -}
{#pointer *ClassItemEntry as ClassItemEntry#}
{- class ClassItemListbox -}
{#pointer *ClassItemListbox as ClassItemListbox#}
{- class ClassListbox -}
{#pointer *ClassListbox as ClassListbox#}
{- class ClassListboxItem -}
{#pointer *ClassListboxItem as ClassListboxItem#}
{- class ClassListboxTextItem -}
{#pointer *ClassListboxTextItem as ClassListboxTextItem#}
{- class ClassListHeader -}
{#pointer *ClassListHeader as ClassListHeader#}
{- class ClassListHeaderSegment -}
{#pointer *ClassListHeaderSegment as ClassListHeaderSegment#}
{- class ClassLogger -}
{#pointer *ClassLogger as ClassLogger#}
{- class ClassMenuBase -}
{#pointer *ClassMenuBase as ClassMenuBase#}
{- class ClassMenuItem -}
{#pointer *ClassMenuItem as ClassMenuItem#}
{- class ClassMultiColumnList -}
{#pointer *ClassMultiColumnList as ClassMultiColumnList#}
{- class ClassMultiLineEditbox -}
{#pointer *ClassMultiLineEditbox as ClassMultiLineEditbox#}
{- class ClassOgreRenderer -}
{#pointer *ClassOgreRenderer as ClassOgreRenderer#}
{- class ClassOgreResourceProvider -}
{#pointer *ClassOgreResourceProvider as ClassOgreResourceProvider#}
{- class ClassProgressBar -}
{#pointer *ClassProgressBar as ClassProgressBar#}
{- class ClassPropertySet -}
{#pointer *ClassPropertySet as ClassPropertySet#}
{- class ClassPushButton -}
{#pointer *ClassPushButton as ClassPushButton#}
{- class ClassRadioButton -}
{#pointer *ClassRadioButton as ClassRadioButton#}
{- class ClassRenderer -}
{#pointer *ClassRenderer as ClassRenderer#}
{- class ClassResourceProvider -}
{#pointer *ClassResourceProvider as ClassResourceProvider#}
{- class ClassScheme -}
{#pointer *ClassScheme as ClassScheme#}
{- class ClassSchemeManager -}
{#pointer *ClassSchemeManager as ClassSchemeManager#}
{- class ClassScriptFunctor -}
{#pointer *ClassScriptFunctor as ClassScriptFunctor#}
{- class ClassScriptModule -}
{#pointer *ClassScriptModule as ClassScriptModule#}
{- class ClassScrollablePane -}
{#pointer *ClassScrollablePane as ClassScrollablePane#}
{- class ClassScrollbar -}
{#pointer *ClassScrollbar as ClassScrollbar#}
{- class ClassScrolledContainer -}
{#pointer *ClassScrolledContainer as ClassScrolledContainer#}
{- class ClassScrolledItemListBase -}
{#pointer *ClassScrolledItemListBase as ClassScrolledItemListBase#}
{- class ClassSlider -}
{#pointer *ClassSlider as ClassSlider#}
{- class ClassSpinner -}
{#pointer *ClassSpinner as ClassSpinner#}
{- class ClassSystem -}
{#pointer *ClassSystem as ClassSystem#}
{- class ClassTabButton -}
{#pointer *ClassTabButton as ClassTabButton#}
{- class ClassThumb -}
{#pointer *ClassThumb as ClassThumb#}
{- class ClassTooltip -}
{#pointer *ClassTooltip as ClassTooltip#}
{- class ClassTree -}
{#pointer *ClassTree as ClassTree#}
{- class ClassUDim -}
{#pointer *ClassUDim as ClassUDim#}
{- class ClassUVector2 -}
{#pointer *ClassUVector2 as ClassUVector2#}
{- class ClassWidgetLookManager -}
{#pointer *ClassWidgetLookManager as ClassWidgetLookManager#}
{- class ClassWindow -}
{#pointer *ClassWindow as ClassWindow#}
{- class ClassWindowManager -}
{#pointer *ClassWindowManager as ClassWindowManager#}
{- class ClassXMLParser -}
{#pointer *ClassXMLParser as ClassXMLParser#}
{- class ClassHG3DEventController -}
{#pointer *ClassHG3DEventController as ClassHG3DEventController#}
{- class ClassHG3DEventStaticFunctions -}
{#pointer *ClassHG3DEventStaticFunctions as ClassHG3DEventStaticFunctions#}
{- class ClassHG3DListboxStaticFunctions -}
{#pointer *ClassHG3DListboxStaticFunctions as ClassHG3DListboxStaticFunctions#}
{- class ClassHG3DWindowStaticFunctions -}
{#pointer *ClassHG3DWindowStaticFunctions as ClassHG3DWindowStaticFunctions#}
{- class ClassSystemHG3D -}
{#pointer *ClassSystemHG3D as ClassSystemHG3D#}
{- class ClassWindowManagerHG3D -}
{#pointer *ClassWindowManagerHG3D as ClassWindowManagerHG3D#}
