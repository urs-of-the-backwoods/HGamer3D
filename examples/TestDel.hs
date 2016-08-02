{-# LANGUAGE OverloadedStrings #-}
{-
:l RotatingCube
main
-}

module Main where

import HGamer3D

import qualified Data.Text as T
import Control.Concurrent
import Control.Monad
import System.Exit

gameLogic hg3d = do

      -- create camera
      eCam <- newE hg3d [
            ctCamera #: FullViewCamera,
            ctPosition #: Vec3 1 1 (-30.0),
            ctLight #: Light PointLight 1.0 1000.0 1.0 
            ]

      eText <- newE hg3d [
            ctText #: "Rotating Cube Example",
            ctScreenRect #: Rectangle 10 10 100 25
            ]

      -- CH5-1s
      eButton <- newE hg3d [
            ctButton #: Button False "Exit",
            ctScreenRect #: Rectangle 200 10 50 25
            ]

      eButtonNew <- newE hg3d [
            ctButton #: Button False "New",
            ctScreenRect #: Rectangle 200 70 50 25
            ]

      eButtonDel <- newE hg3d [
            ctButton #: Button False "Del",
            ctScreenRect #: Rectangle 200 130 50 25
            ]

      eGeo <- newE hg3d [
            ctGeometry #: ShapeGeometry Cube,
            ctMaterial #: matBlue,
            ctScale #: Vec3 10.0 10.0 10.0,
            ctPosition #: Vec3 0.0 0.0 0.0,
            ctOrientation #: unitU
            ]

      varGeo <- makeVar (Just eGeo)

      let createCube = do

            eGeo' <- newE hg3d [
                  ctGeometry #: ShapeGeometry Cube,
                  ctMaterial #: matBlue,
                  ctScale #: Vec3 10.0 10.0 10.0,
                  ctPosition #: Vec3 0.0 0.0 0.0,
                  ctOrientation #: unitU
                  ]
            idE eGeo' >>= print
            writeVar varGeo (Just eGeo')
            return ()

      let delCube = do
            eGeo' <- writeVar varGeo Nothing
            case eGeo' of
                  Just g -> delE g >> return ()
                  Nothing -> return ()

      registerCallback hg3d eButton ctButton (\(Button flag _) -> if not flag then exitHG3D hg3d else return ())
      registerCallback hg3d eButtonNew ctButton (\(Button flag _) -> if not flag then createCube else return ())
      registerCallback hg3d eButtonDel ctButton (\(Button flag _) -> if not flag then delCube else return ())
      -- CH5-1e

      -- rotate the cube
      let rotateZ = do
            forever $ do 
                  eGeo' <- readVar varGeo
                  case eGeo' of
                        Just g -> updateC g ctOrientation (\u -> (rotU vec3Z 0.021) .*. u)
                        Nothing -> return ()
                  sleepFor (msecT 12)
            return ()


      -- CH4-2s
      let rotateX = do
            forever $ do 
                  eGeo' <- readVar varGeo
                  case eGeo' of
                        Just g -> updateC g ctOrientation (\u -> (rotU vec3X 0.012) .*. u)
                        Nothing -> return ()
                  sleepFor (msecT 16)
            return ()
-- CH4-2e


      forkIO $ rotateZ

      forkIO $ rotateX

      return ()


main = do 
      runGame standardGraphics3DConfig gameLogic (msecT 20)
      return ()
