{-# LANGUAGE OverloadedStrings #-}

module Sample_05_MouseModes where

import HGamer3D

import qualified Data.Text as T
import Control.Concurrent
import Control.Monad
import SampleRunner

createAll hg3d = do

    -- create all elements
    res <- mapM (newE hg3d) [

            [
                ctMouseConfig #: MouseConfig Absolute
            ]

            ,[
                ctInputEventHandler #: DefaultEventHandler, 
                ctKeyEvent #: NoKeyEvent,
                ctMouseEvent #: NoMouseEvent
            ]

            ,[
                ctStaticText #: "Keys: A - Absolute Mouse Mode, R - Relative Mouse Mode, W - Wrap Mouse Mode",
                ctScreenRect #: ScreenRect 10 110 200 10
            ]

            ,[
                ctStaticText #: "Mouse Mode Set To ...",
                ctScreenRect #: ScreenRect 10 125 200 10
            ]

            ,[
                ctStaticText #: "Mouse Event",
                ctScreenRect #: ScreenRect 10 140 200 10
            ]

        ]

    return res


showMode mode txtMode quitV = do
    q <- readVar quitV
    m <- readC mode ctMouseConfig
    setC txtMode ctStaticText (T.pack (show m))
    sleepFor (msecT 20)
    if not q
      then showMode mode txtMode quitV
      else return ()

addMouseEventCallback hg3d event txtEvent = registerCallback hg3d event ctMouseEvent (\evt -> setC txtEvent ctStaticText (T.pack (show evt))) 
addKeyEventCallback hg3d event mode = registerCallback hg3d event ctKeyEvent (\evt -> case evt of
                                                                                            KeyUpEvent (KeyData _ _ "A") -> setC mode ctMouseConfig (MouseConfig Absolute)
                                                                                            KeyUpEvent (KeyData _ _ "R") -> setC mode ctMouseConfig (MouseConfig Relative)
                                                                                            KeyUpEvent (KeyData _ _ "W") -> setC mode ctMouseConfig (MouseConfig Wrap) 
                                                                                            _ -> return ())

creator hg3d = do
    res <- createAll hg3d
    let [mode, event, txtIntro, txtMode, txtEvent] = res
    quitV <- makeVar False
    forkIO $ showMode mode txtMode quitV
    addMouseEventCallback hg3d event txtEvent
    addKeyEventCallback hg3d event mode
    return (res, quitV)

destructor (res, quitV) = do
  writeVar quitV True
  sleepFor (msecT 500)
  let [mode, event, txtIntro, txtMode, txtEvent] = res
  mapM delE [event]
  mapM delE [mode, txtIntro, txtMode, txtEvent]
  return ()

sampleRunner hg3d = SampleRunner (return ()) (do
                                    state <- creator hg3d
                                    return (SampleRunner (destructor state) (return emptySampleRunner) ))

