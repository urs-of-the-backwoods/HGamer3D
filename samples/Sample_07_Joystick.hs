{-# LANGUAGE OverloadedStrings #-}

module Sample_07_Joystick where

import HGamer3D

import qualified Data.Text as T
import Control.Concurrent
import Control.Monad
import SampleRunner

creator hg3d = do

    joy <- newE hg3d [
               ctJoystick #: Joystick 0
               ,ctJoystickEvent #: NoJoystickEvent
            ]

    text1 <- newE hg3d [
              ctStaticText #: "Joystick Events (only shown if fired):"
              , ctScreenRect #: ScreenRect 10 80 400 25
              ]

    text2 <- newE hg3d [
              ctStaticText #: ""
              , ctScreenRect #: ScreenRect 10 110  400 25
              ]

    registerCallback hg3d joy ctJoystickEvent (\val -> setC text2 ctStaticText (T.pack . show $ val))

    return (joy, text1, text2)

destructor (joy, text1, text2) = do
  delE joy
  delE text1
  delE text2
  return ()

sampleRunner hg3d = SampleRunner (return ()) (do
                                    state <- creator hg3d
                                    return (SampleRunner (destructor state) (return emptySampleRunner) ))

