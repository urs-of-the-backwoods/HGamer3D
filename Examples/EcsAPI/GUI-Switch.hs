{-# LANGUAGE StandaloneDeriving, DeriveDataTypeable #-}

module Main

where

import HGamer3D.Data
import HGamer3D.Engine.EcsAPI
import HGamer3D.Graphics3D.EcsAPI
import HGamer3D.GUI.EcsAPI

import Control.Concurrent

-- define a camera and some light
camera1 = Camera (Frustum 5.0 5000.0 (Deg 60)) (Viewport 0 (Rectangle 0.0 0.0 1.0 1.0) black)
light1 = Light white white PointLight 

-- define some test geometry
f0 = SimpleFigure Sphere (ResourceMaterial "Colours/Red")
f1 = SimpleFigure Sphere (ResourceMaterial "Colours/Blue")
f2 = SimpleFigure Sphere (ResourceMaterial "Colours/Green")
f3 = SimpleFigure Cube (ResourceMaterial "Colours/Red")
f4 = SimpleFigure Cube (ResourceMaterial "Colours/Blue")
f5 = SimpleFigure Cube (ResourceMaterial "Colours/Green")

figure1 = CombinedFigure [
     (Vec3 200.0 0.0 0.0, unitU, unitVec3 &* 0.3, f0),
     (Vec3 0.0 200.0 0.0, unitU, unitVec3 &* 0.3 , f1),
     (Vec3 0.0 0.0 200.0, unitU, unitVec3 &* 0.3 , f2),
     (Vec3 (-200.0) 0.0 0.0, unitU, unitVec3 &* 0.3, f3),
     (Vec3 0.0 (-200.0) 0.0, unitU, unitVec3 &* 0.3 , f4),
     (Vec3 0.0 0.0 (-200.0), unitU, unitVec3 &* 0.3 , f5)
  ]

-- define some gui layouts
textWindow = Form "Vanilla"
                (LayoutFC (Window "Text Window" [
                                      Text "GUI Switch Example - Text Window",
                                      XPos (GUIDim 0.1 0.0),
                                      YPos (GUIDim 0.1 0.0),
                                      Width (GUIDim 0.0 300.0),
                                      Height (GUIDim 0.0 400.0)
                                  ]) [

                       ( 
                             LayoutFC (VerticalLayout   [  
                                                               XPos (GUIDim 0.0 0.0),
                                                               YPos (GUIDim 0.0 0.0),
                                                               Width (GUIDim 0.0 300.0),
                                                               Height (GUIDim 0.0 400.0)
                                                             ])
                             [
                               (WidgetFC (MultilineText "myMultiLineText" [
                                                               Margin (GUIDim 0.0 5.0),
                                                               Text "This is a cool\nMultiline textbox\nwhich is editable",
                                                               Width (GUIDim 0.0 200.0),
                                                               Height (GUIDim 0.0 300.0)      
                                                                                   ] ))
                             ]
                          )])
                
textWindow2 = Form "WindowsLook"
                (LayoutFC (VerticalLayout  [
                                      Text "GUI Switch Example - Text Window",
                                      XPos (GUIDim 0.1 0.0),
                                      YPos (GUIDim 0.1 0.0),
                                      Width (GUIDim 0.0 300.0),
                                      Height (GUIDim 0.0 400.0)
                                  ]) [

                       ( 
                             LayoutFC (VerticalLayout   [  
                                                               XPos (GUIDim 0.0 0.0),
                                                               YPos (GUIDim 0.0 0.0),
                                                               Width (GUIDim 0.0 300.0),
                                                               Height (GUIDim 0.0 400.0)
                                                             ])
                             [
                               (WidgetFC (MultilineText "myMultiLineText" [
                                                               Margin (GUIDim 0.0 5.0),
                                                               Text "This is a cool\nMultiline textbox\nwhich is editable",
                                                               Width (GUIDim 0.0 200.0),
                                                               Height (GUIDim 0.0 300.0)      
                                                                                   ] ))
                             ]
                          )])
                
buttonBlock = Form "Vanilla" (LayoutFC (VerticalLayout [
                                                               XPos (GUIDim 0.7 0.0),
                                                               YPos (GUIDim 0.1 0.0),
                                                               Width (GUIDim 0.3 0.0),
                                                               Height (GUIDim 0.5 0.0)
                                                         ])
                              [
                                (WidgetFC (Button "B-GUI1" [
                                                               Margin (GUIDim 0.0 5.0),
                                                               Text "GUI One",
                                                               Width (GUIDim 0.3 0.0),
                                                               Height (GUIDim 0.0 20.0)
                                                   ] )),
                                (WidgetFC (Button "B-GUI2" [
                                                               Margin (GUIDim 0.0 5.0),
                                                               Text "GUI Two",
                                                               Width (GUIDim 0.3 0.0),
                                                               Height (GUIDim 0.0 20.0)
                                                   ] )),
                               (WidgetFC (MultilineText "outText" [
                                                               Margin (GUIDim 0.0 5.0),
                                                               Width (GUIDim 0.3 0.0),
                                                               Height (GUIDim 0.5 0.0),
                                                               Text "out text to be done"
                                                                                   ] ))
                              ] )

-- create entities, which are needed

makeEs = do
  -- camera, light and scene parameters into one
  camE <- entity [
       CTCam #: camera1,
       CTLig #: light1,
       CTPos #: Vec3 0.0 0.0 0.0,
       CTScP #: SceneParameter white NoShadows NoSky
       ]

  -- button gui into one
  bE <- entity [
     CTGFo #: buttonBlock
     ]

  -- switchable gui into the other, re-used as event receiver for form events
  guiE <- entity [
    CTGFo #: textWindow,
    CTEvR #: EventReceiver [FormEvents] 
    ]

  return [camE, bE, guiE]

checkEvents guiE bE = do
  evts <- receiveEvents guiE
  mapM (\evt -> case evt of
           (FormEvt (FormButtonClick "B-GUI1")) -> updateC (guiE # CTGFo) (const textWindow)
           (FormEvt (FormButtonClick "B-GUI2")) -> updateC (guiE # CTGFo) (const textWindow2)
           (FormEvt (FormValueChange "myMultiLineText" _))  -> sendEvent bE (FormEvt (FormSetValue [("outText", (FVS (show evt)))]))
           _ -> return ()
       ) evts
  threadDelay (msec (msecT 50))
  checkEvents guiE bE
         

main = do
  
  ecsG3D <- runSystemGraphics3D (msecT 30)
  ecsEvt <- runSystemEvent (msecT 110)
  let systems = ecsG3D #+ ecsEvt #+ []
  [camE, bE, guiE] <- makeEs
  mapM (addToWorld systems) [camE, bE, guiE] 
  checkEvents guiE bE
  return ()

