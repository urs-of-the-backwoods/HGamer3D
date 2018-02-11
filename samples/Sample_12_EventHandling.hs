{-# LANGUAGE OverloadedStrings #-}

module Sample_12_EventHandling where

import HGamer3D
import Control.Concurrent
import Control.Monad
import SampleRunner
import Data.Text as T

creator hg3d = do
  es <- newET hg3d [
    "eK" <: [
        ctKeyEvent #: NoKeyEvent
        ],

    "eS" <: [
        ctScreenModeEvent #: ScreenModeEvent 0 0 False False
        ],

     "txt" <: [
        ctStaticText #: "",
        ctScreenRect #: ScreenRect 10 100 200 35
              ],

     "txt2" <: [
        ctStaticText #: "",
        ctScreenRect #: ScreenRect 10 150 200 35
              ]
     ]

  registerCallback hg3d (es # "eK") ctKeyEvent (\evt -> do
                                                   setC (es # "txt") ctStaticText ("key event")
                                                   setC (es # "txt2") ctStaticText (T.pack (show evt))
                                                   return ()
                                               )

  registerCallback hg3d (es # "eS") ctScreenModeEvent (\evt -> do
                                                   setC (es # "txt") ctStaticText ("screen mode event")
                                                   setC (es # "txt2") ctStaticText (T.pack (show evt))
                                                   return ()
                                                 )

  return es

destructor es = do
  delET es
  return ()

sampleRunner hg3d = SampleRunner (return ()) (do
                                    state <- creator hg3d
                                    return (SampleRunner (destructor state) (return emptySampleRunner) ))

