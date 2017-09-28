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
    entities <- newET hg3d [
        "events" <: [
            ctInputEvents #: ()
            , ctUIClickEvent #: NoClick
        ], 

        "window" <| ([
            ctWindowGUI #: ()
            , ctLayout #: Layout LMVertical 10 (ScreenRect2 6 6 6 6) 
            , ctPosition2D #: IntVec2 0 0
            , ctAlignment #: Alignment HACenter VACenter
            , ctMinSize #: MinSize 384 0
            , ctName #: "Window"
            , ctUIHoverEvent #: NoHover
            , ctUIDragEvent #: NoDrag
            ] , [

                () -| ([
                        ctUIElement #: ()
                        , ctMinSize #: MinSize 0 24
                        , ctLayout #: Layout LMHorizontal 0 (ScreenRect2 0 0 0 0)
                        , ctAlignment #: Alignment HALeft VATop
                        ], [
                        () -: [
                            ctStaticText #: "Hello GUI!"
                            , ctName #: "Title Text"
                            ],

                        "button" <: [
                            ctButton #: Button False ""
                            , ctName #: "Close Button"
                            , ctUIStyle #: "CloseButton"
                            ]   
                        ])
                , "checkbox" <: [
                    ctCheckBox #: False
                    , ctMinSize #: MinSize 0 24
                    , ctName #: "CheckBox"
                    ]
                , "button" <: [
                    ctButton #: Button False "Button"
                    , ctMinSize #: MinSize 0 24
                    , ctName #: "Button"
                    ]
                , "edittext" <: [
                    ctEditText #: ""
                    , ctMinSize #: MinSize 0 24
                    , ctName #: "EditText"
                    ]
            ])
        ]

    registerCallback hg3d (entities # "window") ctUIDragEvent (\de -> case de of
                                                                    (DragMove _ (IntVec2 dx dy) _ _) -> updateC (entities # "window") ctPosition2D (\(IntVec2 x y) -> (IntVec2 (x + dx) (y + dy))) >> return ()
                                                                    _ -> return ()
                                                                    )

main = do 
    runGame standardGraphics3DConfig gameLogic (msecT 20)
    return ()
