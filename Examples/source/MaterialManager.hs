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

-- Material Manager Example

module Main

where

import HGamer3D.Data
import HGamer3D.Common
import HGamer3D.Graphics3D

import Control.Concurrent
import Data.Maybe
import Data.List

-- the things we need for the rotating entity
camera1 = Camera (Frustum 5.0 5000.0 (Deg 40)) (Viewport 0 (Rectangle 0.0 0.0 1.0 1.0) black)

light1 = Light white white PointLight 

-- build and deconstruct composite figures
toFigure scale geo mat = CombinedFigure [(zeroVec3, unitU, scale, SimpleFigure geo mat)]
fromFigure fig = case fig of 
  CombinedFigure [(zeroVec3, unitU, scale, SimpleFigure geo mat)] -> (scale, geo, mat)
  _ -> error "wrong figure type"

platon = toFigure unitVec3 Dodekaeder (ResourceMaterial "Colours/Red")

rotationAngle :: IO Float
rotationAngle = do
  t <- getTime
  let t' = fromIntegral ((msec t) `mod` 5000)
  let v = (t' * 6.28 / 5000.0) 
  return v

-- define gui layout for radio button switch selection on the left
shapeSelect = Form "Vanilla"
                (LayoutFC (Window "Select Shape" [
                                      Text "Select Shape",
                                      XPos (GUIDim 0.0 0.0),
                                      YPos (GUIDim 0.1 0.0),
                                      Width (GUIDim 0.2 0.0),
                                      Height (GUIDim 0.5 0.0)
                                  ]) [

                       ( 
                             LayoutFC (VerticalLayout   [ 
                                Width (GUIDim 0.2 0.0),
                                Height (GUIDim 0.5 0.0)
                                                             ])
                             [
                               (WidgetFC (RadioButton "Sphere" [
                                  Margin (GUIDim 0.0 5.0),
                                  Text "Sphere",
                                  Width (GUIDim 1.0 0.0),
                                  Height (GUIDim 0.0 20.0),
                                  Selected False
                                  ] )),
                               (WidgetFC (RadioButton "Cube" [
                                  Margin (GUIDim 0.0 5.0),
                                  Text "Cube",
                                  Width (GUIDim 1.0 0.0),
                                  Height (GUIDim 0.0 20.0),
                                  Selected False
                                  ] )),
                               (WidgetFC (RadioButton "Dodekaeder" [
                                  Margin (GUIDim 0.0 5.0),
                                  Text "Dodekaeder",
                                  Width (GUIDim 1.0 0.0),
                                  Height (GUIDim 0.0 20.0),
                                  Selected True
                                  ] )),
                               (WidgetFC (RadioButton "Ikosaeder" [
                                  Margin (GUIDim 0.0 5.0),
                                  Text "Ikosaeder",
                                  Width (GUIDim 1.0 0.0),
                                  Height (GUIDim 0.0 20.0),
                                  Selected False
                                  ] ))
                             ]
                          )])

-- define gui layout for material selection window on the right
materialSelect = Form "Vanilla"
                (LayoutFC (Window "Select Material" [
                                      Text "Select Material",
                                      XPos (GUIDim 0.8 0.0),
                                      YPos (GUIDim 0.1 0.0),
                                      Width (GUIDim 0.2 0.0),
                                      Height (GUIDim 0.5 0.0)
                                  ]) [

                       ( 
                             LayoutFC (VerticalLayout   [ 
                                Width (GUIDim 1.0 0.0),
                                Height (GUIDim 1.0 0.0)
                                                             ])
                             [
                               (WidgetFC (ComboBox "Material" [
                                  Margin (GUIDim 0.0 5.0),
                                  Text "<select>",
                                  Width (GUIDim 1.0 0.0),
                                  Height (GUIDim 1.0 0.0),
                                  TextChoice (map fst materials)      
                                  ] ))
                             ]
                          )])

-- define the materials
materials = [
              ("Red", ResourceMaterial "Colours/Red"),
              ("Green", ResourceMaterial "Colours/Green"),
              ("Blue", ResourceMaterial "Colours/Blue")
            ]

-- main program
main = do

  -- camera, light and scene parameters into one
  envE <- newE [
       CTCam #: camera1,
       CTScP #: SceneParameter white NoShadows NoSky,
       CTWin #: ()
       ]

  -- geometry and light entities
  geoE <- newE [
     CTPos #: Vec3 0.0 0.0 (-20.0),
     CTOri #: unitU,
     CTFig #: platon
     ]
  liE1 <- newE [
       CTLig #: light1,
       CTPos #: Vec3 500.0 500.0 (-500.0)
       ]
  liE2 <- newE [
       CTLig #: light1,
       CTPos #: Vec3 100.0 50.0 0.0	
       ]

  -- the gui entities
  leftGuiE <- newE [
     CTGFo #: shapeSelect
     ]
  rightGuiE <- newE [
     CTGFo #: materialSelect
     ]

  -- run graphics system
  world <- forkGraphics3DWorld (msecT 30)

  -- add entities to running system
  mapM (addToWorld world) [envE, geoE, liE1, liE2, leftGuiE, rightGuiE] 

  -- changing a material
  let materialChange [(_, FVS mat)] = updateE geoE CTFig (\fig -> let (scale, geo, _) = fromFigure fig in toFigure scale geo (fromJust (lookup mat materials)))

  -- change shape
  let doshape name = case name of
           "Sphere" -> updateE geoE CTFig (\fig -> let (scale, geo, mat) = fromFigure fig in toFigure (unitVec3 &* 0.05) Sphere mat)
           "Cube" -> updateE geoE CTFig (\fig -> let (scale, geo, mat) = fromFigure fig in toFigure (unitVec3 &* 0.03) Cube mat)
           "Dodekaeder" -> updateE geoE CTFig (\fig -> let (scale, geo, mat) = fromFigure fig in toFigure (unitVec3) Dodekaeder mat)
           "Ikosaeder" -> updateE geoE CTFig (\fig -> let (scale, geo, mat) = fromFigure fig in toFigure (unitVec3 &* 2.0) Ikosaeder mat)
           _ -> return ()
  let shapeChange vals = mapM (\(name, FVB flag) -> if flag then doshape name else return ()) vals >> return ()

  -- event handling routine
  let evtCallback evts = do
                  mapM (\evt -> do
                                  case evt of 
                                       FormEvt (FormValueChange "Material" vals) -> materialChange vals
                                       FormEvt (FormValueChange _ vals) -> shapeChange vals
                                       _ -> return () ) evts
                  return ()

  -- implement quit handler
  qvar <- regQuitHandler envE

  -- implement gui callbacks
  regEvtH leftGuiE evtCallback
  regEvtH rightGuiE evtCallback

  -- main run loop
  let loop = do
      sleepFor (msecT 50)
      rangle <- rotationAngle
      updateE geoE CTOri (const (rotU vec3Y rangle))
      loop

  -- run loop
  forkIO loop
  takeMVar qvar
         

