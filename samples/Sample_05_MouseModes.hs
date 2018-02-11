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
                ctMouse #: MMAbsolute,
                ctMouseEvent #: NoMouseEvent
            ]

            ,[
                ctKeyEvent #: NoKeyEvent
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


showMode mouse txtMode txtEvent varText quitV = do
    q <- readVar quitV
    m <- readC mouse ctMouse
    t <- readVar varText
    setC txtEvent ctStaticText (T.pack t)
    setC txtMode ctStaticText (T.pack (show m))
    sleepFor (msecT 20)
    if not q
      then showMode mouse txtMode txtEvent varText quitV
      else return ()

addMouseEventCallback hg3d mouse varText = registerCallback hg3d mouse ctMouseEvent (\evt ->  writeVar varText (show evt) >> return ()) 
addKeyEventCallback hg3d keys mouse = registerCallback hg3d keys ctKeyEvent (\evt -> case evt of
                                                                                            KeyUpEvent (KeyData _ _ "A") -> setC mouse ctMouse MMAbsolute
                                                                                            KeyUpEvent (KeyData _ _ "R") -> setC mouse ctMouse MMRelative
                                                                                            KeyUpEvent (KeyData _ _ "W") -> setC mouse ctMouse MMWrap 
                                                                                            _ -> return ())

creator hg3d = do
    (varText, res) <- createAll hg3d
    let [mouse, keys, txtIntro, txtMode, txtEvent] = res
    quitV <- makeVar False
    forkIO $ showMode mouse txtMode txtEvent varText quitV
    addMouseEventCallback hg3d mouse varText
    addKeyEventCallback hg3d keys mouse
    return (res, quitV)

destructor (res, quitV) = do
  writeVar quitV True
  sleepFor (msecT 300)
  mapM delE res
  return ()

sampleRunner hg3d = SampleRunner (return ()) (do
                                    state <- creator hg3d
                                    return (SampleRunner (destructor state) (return emptySampleRunner) ))

