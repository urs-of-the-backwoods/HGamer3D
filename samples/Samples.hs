{-# LANGUAGE OverloadedStrings #-}

module Main where

import HGamer3D
import Control.Concurrent
import Control.Monad

import qualified Sample_01_RotatingCube as RC
import qualified Sample_02_Materials as M
import qualified Sample_03_Hierarchy as H
import qualified Sample_04_Sound as S
import qualified Sample_05_MouseModes as MM
import qualified Sample_06_GUI as G
import qualified Sample_07_Joystick as JS
import qualified Sample_08_SceneLoad as SL
import qualified Sample_09_Skybox as SB
import qualified Sample_10_3DText as T

import SampleRunner

switchRunner var runner = do
  previousRunner <- readVar var
  getStopFunction previousRunner -- stops the old
  newRunner <- getRunning runner -- starts the new
  writeVar var newRunner
  return ()



gameLogic hg3d = do

      currentSampleRunner <- makeVar emptySampleRunner

      -- create camera
      cam <- newE hg3d [
                  ctCamera #: FullViewCamera,
                  ctPosition #: Vec3 1 1 (-30.0),
                  ctLight #: Light PointLight 1.0 1000.0 1.0,
                  ctOrientation #: unitU
                  ]

      let sampleData = [
            ("01 Rotating Cube", RC.sampleRunner hg3d)
            , ("02 Materials", M.sampleRunner hg3d cam)
            , ("03 Hierarchy of Elements", H.sampleRunner hg3d)
            , ("04 Sound", S.sampleRunner hg3d)
            , ("05 Mouse Modes", MM.sampleRunner hg3d)
            , ("06 GUI", G.sampleRunner hg3d)
            , ("07 Joystick", JS.sampleRunner hg3d)
            , ("08 Loading a Scene", SL.sampleRunner hg3d)
            , ("09 Skybox", SB.sampleRunner hg3d cam)
            , ("10 3D Text", T.sampleRunner hg3d)
            , ("Clear", emptySampleRunner)
            ]

      es <- newET hg3d [

            -- create title text
            () -: [
                  ctStaticText #: "HGamer3D Sample Browser",
                  ctScreenRect #: ScreenRect 10 10 100 25
                  ],

            -- create dropdown menu, to select sample
            "eSelector" <: [
                  ctDropDownList #: DropDownList (map fst sampleData) NoSelection,
                  ctScreenRect #: ScreenRect 10 40 300 20
                  ],

            -- create logo
            () -: [
                  ctSprite #: Sprite "Textures/hg3dlogo.png" 0.8,
                  ctScreenRect #: ScreenRect 669 10 121 121
                  ],

            -- create button
            "eButton" <: [
                  ctButton #: Button False "Exit",
                  ctScreenRect #: ScreenRect 740 550 50 25
                  ]

            ]

      registerCallback hg3d (es # "eButton") ctButton (\(Button flag _) -> if not flag then exitHG3D hg3d else return ())
      registerCallback hg3d (es # "eSelector") ctDropDownList (\(DropDownList _ selection) -> case selection of
                                                                  Selection t -> switchRunner currentSampleRunner (snd (sampleData !! t)) 
                                                                  NoSelection -> return () )

      return ()

main = do 
      runGame standardGraphics3DConfig gameLogic (msecT 20)
      return ()
