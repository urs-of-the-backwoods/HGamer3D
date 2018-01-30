{-# LANGUAGE OverloadedStrings #-}

module Sample_06_GUI where

import HGamer3D

import qualified Data.Text as T
import Control.Concurrent
import Control.Monad
import SampleRunner

createEntities hg3d = do
    res <- mapM (newE hg3d) [
           [   -- descriptive text
                ctStaticText #: "A Button: \n"
                , ctScreenRect #: ScreenRect 10 110 120 25
            ]
           ,[   -- button
                ctButton #: Button False "Press Me"
                , ctScreenRect #: ScreenRect 130 110 100 25
            ]
           ,[   -- descriptive text
                ctStaticText #: "A Checkbox: \n"
                , ctScreenRect #: ScreenRect 10 140 120 25
            ]
           ,[   -- checkbox
                ctCheckBox #: False
                , ctScreenRect #: ScreenRect 130 140 15 15
            ]
           ,[   -- descriptive text
                ctStaticText #: "An EditText: \n"
                , ctScreenRect #: ScreenRect 10 170 120 25
            ]
           ,[   -- edittext
                ctEditText #: ""
                , ctScreenRect #: ScreenRect 130 170 150 25
            ]
           ,[   -- descriptive text
                ctStaticText #: "A Slider: \n"
                , ctScreenRect #: ScreenRect 10 200 120 25
            ]
           ,[   -- slider
                ctSlider #: Slider 100 10
                , ctScreenRect #: ScreenRect 130 200 150 25
            ]
-- CH6-1s    
           ,[   -- descriptive text
                ctStaticText #: "A DropDownList: \n"
                , ctScreenRect #: ScreenRect 10 230 120 25
            ]
           ,[   -- dropdownlist
                ctDropDownList #: DropDownList ["hi", "you"] NoSelection
                , ctScreenRect #: ScreenRect 130 230 150 25
            ]
-- CH6-1e
           ,[   -- output text
                ctStaticText #: "Output Events\n"
                , ctScreenRect #: ScreenRect 10 350 600 300
            ]
        ]

    return res


startPrintEvents res quitV = do
    let [_, button, _, checkbox, _, edittext, _, slider, _, dropdownlist, output] = res

    let printEvents = do
                textOut <- do
                             t1 <- readC button ctButton
                             t2 <- readC checkbox ctCheckBox
                             t3 <- readC edittext ctEditText
                             t4 <- readC slider ctSlider
                             t5 <- readC dropdownlist ctDropDownList
                             return (T.pack ("button: " ++ (show t1) ++ "\ncheckbox: " ++ (show t2) ++ "\nedittext: " ++ (show t3) ++ "\nslider: " ++ (show t4) ++ "\ndropdownlist: " ++ (show t5) ++ "\n\n"))
                setC output ctStaticText textOut
                sleepFor (msecT 200)
                q <- readVar quitV
                if not q
                  then printEvents 
                  else return ()
                return ()

    forkIO $ printEvents


creator hg3d = do
    res <- createEntities hg3d
    quitV <- makeVar False
    startPrintEvents res quitV
    return (res, quitV)

destructor (res, quitV) = do
  writeVar quitV True
  sleepFor (msecT 500)
  mapM delE res
  return ()

sampleRunner hg3d = SampleRunner (return ()) (do
                                    state <- creator hg3d
                                    return (SampleRunner (destructor state) (return emptySampleRunner) ))
