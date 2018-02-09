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

    varText <- makeVar ""
    return (varText, res)


showMode mode txtMode txtEvent varText quitV = do
    q <- readVar quitV
    m <- readC mode ctMouseConfig
    t <- readVar varText
    setC txtEvent ctStaticText (T.pack t)
    setC txtMode ctStaticText (T.pack (show m))
    sleepFor (msecT 20)
    if not q
      then showMode mode txtMode txtEvent varText quitV
      else return ()

addMouseEventCallback hg3d event varText = registerCallback hg3d event ctMouseEvent (\evt ->  writeVar varText (show evt) >> return ()) 
addKeyEventCallback hg3d event mode = registerCallback hg3d event ctKeyEvent (\evt -> case evt of
                                                                                            KeyUpEvent (KeyData _ _ "A") -> setC mode ctMouseConfig (MouseConfig Absolute)
                                                                                            KeyUpEvent (KeyData _ _ "R") -> setC mode ctMouseConfig (MouseConfig Relative)
                                                                                            KeyUpEvent (KeyData _ _ "W") -> setC mode ctMouseConfig (MouseConfig Wrap) 
                                                                                            _ -> return ())

creator hg3d = do
    (varText, res) <- createAll hg3d
    let [mode, event, txtIntro, txtMode, txtEvent] = res
    quitV <- makeVar False
    forkIO $ showMode mode txtMode txtEvent varText quitV
    addMouseEventCallback hg3d event varText
    addKeyEventCallback hg3d event mode
    return (res, quitV)

destructor (res, quitV) = do
  writeVar quitV True
  sleepFor (msecT 300)
  let [mode, event, txtIntro, txtMode, txtEvent] = res
  delE event
  delE mode
  mapM delE [txtIntro, txtMode, txtEvent]
  return ()

sampleRunner hg3d = SampleRunner (return ()) (do
                                    state <- creator hg3d
                                    return (SampleRunner (destructor state) (return emptySampleRunner) ))

