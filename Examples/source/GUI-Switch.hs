{-# LANGUAGE StandaloneDeriving, DeriveDataTypeable #-}
-- This source file is part of HGamer3D
-- (A project to enable 3D game development in Haskell)
-- For the latest info, see http://www.hgamer3d.org
--
-- (c) 2014 Peter Althainz
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

module Main

where

import HGamer3D.Data
import HGamer3D.Common
import HGamer3D.Graphics3D

import Control.Concurrent

-- define a camera and some light
camera1 = Camera (Frustum 5.0 5000.0 (Deg 60)) (Viewport 0 (Rectangle 0.0 0.0 1.0 1.0) black)
light1 = Light white white PointLight 

-- define some test geometry
f0 = SimpleFigure Sphere (ResourceMaterial "Colours/Red")
f1 = SimpleFigure Sphere (ResourceMaterial "Colours/Blue")
f2 = SimpleFigure Sphere (ResourceMaterial "Colours/Green")
f3 = SimpleFigure Cube (ResourceMaterial "Colours/Red")
f4 = SimpleFigure Cube (ResourceMaterial "Colours/Blue")
f5 = SimpleFigure Cube (ResourceMaterial "Colours/Green")

figure1 = CombinedFigure [
     (Vec3 200.0 0.0 0.0, unitU, unitVec3 &* 0.3, f0),
     (Vec3 0.0 200.0 0.0, unitU, unitVec3 &* 0.3 , f1),
     (Vec3 0.0 0.0 200.0, unitU, unitVec3 &* 0.3 , f2),
     (Vec3 (-200.0) 0.0 0.0, unitU, unitVec3 &* 0.3, f3),
     (Vec3 0.0 (-200.0) 0.0, unitU, unitVec3 &* 0.3 , f4),
     (Vec3 0.0 0.0 (-200.0), unitU, unitVec3 &* 0.3 , f5)
  ]

-- define some gui layouts
textWindow = Form "Vanilla"
                (LayoutFC (Window "Text Window" [
                                      Text "GUI Switch Example - Text Window",
                                      XPos (GUIDim 0.1 0.0),
                                      YPos (GUIDim 0.1 0.0),
                                      Width (GUIDim 0.0 300.0),
                                      Height (GUIDim 0.0 400.0)
                                  ]) [

                       ( 
                             LayoutFC (VerticalLayout   [  
                                                               XPos (GUIDim 0.0 0.0),
                                                               YPos (GUIDim 0.0 0.0),
                                                               Width (GUIDim 0.0 300.0),
                                                               Height (GUIDim 0.0 400.0)
                                                             ])
                             [
                               (WidgetFC (MultilineText "myMultiLineText" [
                                                               Margin (GUIDim 0.0 5.0),
                                                               Text "This is a cool\nMultiline textbox\nwhich is editable",
                                                               Width (GUIDim 0.0 200.0),
                                                               Height (GUIDim 0.0 300.0)      
                                                                                   ] ))
                             ]
                          )])
                
textWindow2 = Form "WindowsLook"
                (LayoutFC (VerticalLayout  [
                                      Text "GUI Switch Example - Text Window",
                                      XPos (GUIDim 0.1 0.0),
                                      YPos (GUIDim 0.1 0.0),
                                      Width (GUIDim 0.0 300.0),
                                      Height (GUIDim 0.0 400.0)
                                  ]) [

                       ( 
                             LayoutFC (VerticalLayout   [  
                                                               XPos (GUIDim 0.0 0.0),
                                                               YPos (GUIDim 0.0 0.0),
                                                               Width (GUIDim 0.0 300.0),
                                                               Height (GUIDim 0.0 400.0)
                                                             ])
                             [
                               (WidgetFC (MultilineText "myMultiLineText" [
                                                               Margin (GUIDim 0.0 5.0),
                                                               Text "This is a cool\nMultiline textbox\nwhich is editable",
                                                               Width (GUIDim 0.0 200.0),
                                                               Height (GUIDim 0.0 300.0)      
                                                                                   ] ))
                             ]
                          )])
                
buttonBlock = Form "Vanilla" (LayoutFC (VerticalLayout [
                                                               XPos (GUIDim 0.7 0.0),
                                                               YPos (GUIDim 0.1 0.0),
                                                               Width (GUIDim 0.3 0.0),
                                                               Height (GUIDim 0.5 0.0)
                                                         ])
                              [
                                (WidgetFC (Button "B-GUI1" [
                                                               Margin (GUIDim 0.0 5.0),
                                                               Text "GUI One",
                                                               Width (GUIDim 0.3 0.0),
                                                               Height (GUIDim 0.0 20.0)
                                                   ] )),
                                (WidgetFC (Button "B-GUI2" [
                                                               Margin (GUIDim 0.0 5.0),
                                                               Text "GUI Two",
                                                               Width (GUIDim 0.3 0.0),
                                                               Height (GUIDim 0.0 20.0)
                                                   ] )),
                               (WidgetFC (MultilineText "outText" [
                                                               Margin (GUIDim 0.0 5.0),
                                                               Width (GUIDim 0.3 0.0),
                                                               Height (GUIDim 0.5 0.0),
                                                               Text "out text to be done"
                                                                                   ] ))
                              ] )

-- create entities, which are needed

makeEs = do
  -- camera, light and scene parameters into one
  camE <- newE [
       CTCam #: camera1,
       CTLig #: light1,
       CTPos #: Vec3 0.0 0.0 0.0,
       CTScP #: SceneParameter white NoShadows NoSky
       ]

  -- button gui into one
  bE <- newE [
     CTGFo #: buttonBlock
     ]

  -- switchable gui into the other, re-used as event receiver for form events
  guiE <- newE [
    CTGFo #: textWindow
    ]

  return [camE, bE, guiE]

checkEvents bE guiE evts = do
  mapM (\evt -> case evt of
           (FormEvt (FormButtonClick "B-GUI1")) -> updateE guiE CTGFo (const textWindow)
           (FormEvt (FormButtonClick "B-GUI2")) -> updateE guiE CTGFo (const textWindow2)
           (FormEvt (FormValueChange "myMultiLineText" _))  -> sendCmd bE (FormSetValue [("outText", (FVS (show evt)))])
           _ -> return ()
       ) evts
  return ()
         

main = do
  
  world <- forkGraphics3DWorld (msecT 30)
  [camE, bE, guiE] <- makeEs
  mapM (addToWorld world) [camE, bE, guiE] 
  regEvtH bE (checkEvents bE guiE)

  -- implement quit handler and wait for quit
  qvar <- regQuitHandler camE
  takeMVar qvar
  return ()

