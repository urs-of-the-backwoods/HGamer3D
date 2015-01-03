{-# LANGUAGE DeriveDataTypeable, StandaloneDeriving #-}
{-# OPTIONS_HADDOCK hide #-}
-- This source file is part of HGamer3D
-- (A project to enable 3D game development in Haskell)
-- For the latest info, see http://www.hgamer3d.org
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

-- | Types which describe the GUI Schema Types
module HGamer3D.Graphics3D.GUISchema
where

import Data.Typeable
import HGamer3D.Data as Dat

{- ----------------------------------------------------------------
   GUI Dimensions
   ---------------------------------------------------------------- -}

data GUIDim = GUIDim {
      gdScale :: Float,
      gdOffset :: Float 
      } deriving (Eq, Show, Typeable)

data GUIVec2 = GUIVec2 {
      gv2X :: GUIDim,
      gv2Y :: GUIDim 
}

{- ----------------------------------------------------------------
   GUI Layout
   ---------------------------------------------------------------- -}

data Layout = VerticalLayout  [WidgetProperty]
              | HorizontalLayout [WidgetProperty]
              | GridLayout Int Int [WidgetProperty]
              | Window String [WidgetProperty]
              deriving (Eq, Show, Typeable)


{- ----------------------------------------------------------------
   GUI Widget
   ---------------------------------------------------------------- -}

-- | The data to specify a widget
data Widget = Button String [WidgetProperty]
            | RadioButton String [WidgetProperty]
            | CheckBox String [WidgetProperty]
            | ComboBox String [WidgetProperty]
            | ListBox String [WidgetProperty]
            | EditText String [WidgetProperty]
            | MultilineText String [WidgetProperty]
            | Spinner String [WidgetProperty]
            | Slider String [WidgetProperty]
            deriving (Eq, Show, Typeable)

-- | Widget Properties
data WidgetProperty = XPos GUIDim
                    | YPos GUIDim
                    | Width GUIDim
                    | Height GUIDim
                    | Visible Bool
                    | Alpha Float
                    | Text String
                    | Margin GUIDim
                    | Tooltip String
                    | TextChoice [String]               -- only ComboBox
                    | TextSelection [(String, Bool)]    -- only ListBox
                    | Value Float                       -- only Spinner, Slider
                    | Selected Bool                     -- only CheckBox, RadioButton
                      deriving (Eq, Show, Typeable)

{- ----------------------------------------------------------------
   GUI Form
   ---------------------------------------------------------------- -}

data Form = Form String FormContent deriving (Eq, Show, Typeable)

data FormContent = WidgetFC Widget
                 | LayoutFC Layout [FormContent]
                 deriving (Eq, Show, Typeable)
              
data FormValue = FVF Float
               | FVS String
               | FVTC [String]          -- Text Choice
               | FVTS [(String, Bool)]  -- Text Selection
               | FVB Bool
               | FVE                    -- Empty
                 deriving (Eq, Show, Typeable)
