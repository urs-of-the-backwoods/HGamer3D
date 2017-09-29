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
        ] 

        , "window" <| ([
            ctWindowGUI #: ()
            , ctLayout #: Layout LMVertical 10 (ScreenRect2 6 6 6 6) 
            , ctPosition2D #: IntVec2 0 0
            , ctAlignment #: Alignment HACenter VACenter
            , ctMinSize #: MinSize 384 0
            , ctName #: "Window"
            ] , [

                () -| ([
                        ctUIElement #: ()
                        , ctMinSize #: MinSize 0 24
                        , ctLayout #: Layout LMHorizontal 0 (ScreenRect2 0 0 0 0)
                        , ctAlignment #: Alignment HALeft VATop
                        ], [
                        "titletext" <: [
                            ctStaticText #: "Hello GUI!"
                            , ctName #: "Title Text"
                            ],

                        "close" <: [
                            ctBasicButton #: ""
                            , ctName #: "Close Button"
                            , ctUIStyle #: "CloseButton"
                            , ctButtonEvent #: NoButtonEvent
                            ]   
                        ])
                , "checkbox" <: [
                    ctCheckBox #: False
                    , ctMinSize #: MinSize 0 24
                    , ctName #: "CheckBox"
                    ]
                , "button" <: [
                    ctBasicButton #: "Button"
                    , ctButtonEvent #: NoButtonEvent
                    , ctMinSize #: MinSize 0 24
                    , ctName #: "Button"
                    ]
                , "edittext" <: [
                    ctEditText #: ""
                    , ctMinSize #: MinSize 0 24
                    , ctName #: "EditText"
                    ]
            ])

            , "fish" <| ([
                ctImageButton #: "Textures/UrhoDecal.dds"
                , ctButtonEvent #: NoButtonEvent
                , ctSize2D #: IntVec2 128 128
                , ctPosition2D #: IntVec2 300 50
                , ctName #: "Fish"
                , ctUIDragEvent #: NoDrag
                ], [


                ])
        ]

    let callback idStr evt f = registerCallback hg3d (entities # idStr) evt f

    let changeTitleF es ce = case ce of
            (SingleClick name _ _) -> setC (es # "titletext") ctStaticText (T.concat ["Hello ", name, "!"])
            _ -> return ()

    -- close program on close button
    callback "close" ctButtonEvent (\_ -> exitHG3D hg3d)

    -- inform on clicked item in title bar
    callback "events" ctUIClickEvent (changeTitleF entities) 

    -- drag fish
    callback "fish" ctUIDragEvent (\evt -> case evt of
                (DragMove _ _ (IntVec2 dx dy) _ ) -> updateC (entities # "fish") ctPosition2D (\(IntVec2 x y) -> IntVec2 (x + dx) (y + dy)) 
                _ -> return ()
            )


main = do 
    runGame standardGraphics3DConfig gameLogic (msecT 20)
    return ()
