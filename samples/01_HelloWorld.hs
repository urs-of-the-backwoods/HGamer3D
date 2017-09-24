-- sample 01_HelloWorld


{-# LANGUAGE OverloadedStrings #-}
module Main where

import HGamer3D

import qualified Data.Text as T
import Control.Concurrent
import Control.Monad
import System.Exit

gameLogic hg3d = do


    -- create Hello World Text
    eText <- newE hg3d [
        ctStaticText #: "HGamer3D: Hello World" 
        , ctFont #: Font "Fonts/Anonymous Pro.ttf" 30
        , ctColour  #: Colour 0.0 1.0 0.0 0.0
        , ctAlignment #: Alignment HACenter VACenter
        ]

    return ()

main = do 
    runGame standardGraphics3DConfig gameLogic (msecT 20)
    return ()
