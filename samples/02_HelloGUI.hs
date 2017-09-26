-- sample 02_HelloGUI


{-# LANGUAGE OverloadedStrings #-}
module Main where

import HGamer3D

import qualified Data.Text as T
import Control.Concurrent
import Control.Monad
import System.Exit

gameLogic hg3d = do

    -- create Window
    eWin <- newE hg3d [
        ctWindowGUI #: ()
        , ctLayout #: Layout LMVertical 2 (ScreenRect2 20 20 20 20) 
        , ctMinSize #: MinSize 300 200
        ]

    return ()

main = do 
    runGame standardGraphics3DConfig gameLogic (msecT 20)
    return ()
