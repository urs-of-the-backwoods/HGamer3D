{-# LANGUAGE OverloadedStrings #-}

module Main where

import HGamer3D

import qualified Data.Text as T
import Control.Concurrent
import Control.Monad

createAll hg3d = do

    -- create all elements
    res <- mapM (newE hg3d) [

            [   -- camera
                ctCamera #: FullViewCamera,
                ctPosition #: Vec3 1 1 (-30.0),
                ctLight #: Light PointLight 1.0 1000.0 1.0 
            ]
           ,[   -- blue cube
                ctGeometry #: ShapeGeometry Cube,
                ctMaterial #: matBlue,
                ctScale #: Vec3 5.0 5.0 5.0,
                ctPosition #: Vec3 0.0 0.0 0.0,
                ctOrientation #: unitU
            ]
            
            -- SOUND BUTTONS AND SOUNDSOURCE
            
           ,[   -- metal-clash button and sound
                ctButton #: Button False "metal clash"
                , ctScreenRect #: Rectangle 10 10 150 50
                , ctSoundSource #: Sound "Sounds/inventory_sound_effects/metal-clash.wav" 1.0 False "Sounds"
                , ctPlayCmd #: Stop
            ]
-- CH7-1s
           ,[   -- ring-inventory button and sound
                ctButton #: Button False "ring inventory"
                , ctScreenRect #: Rectangle 180 10 150 50
                , ctSoundSource #: Sound "Sounds/inventory_sound_effects/ring_inventory.wav" 1.0 False "Sounds"
                , ctPlayCmd #: Stop
            ]
-- CH7-1e
           ,[   -- sell_buy_item button and sound
                ctButton #: Button False "sell buy item"
                , ctScreenRect #: Rectangle 350 10 150 50
                , ctSoundSource #: Sound "Sounds/inventory_sound_effects/sell_buy_item.wav" 1.0 False "Sounds"
                , ctPlayCmd #: Stop
            ]
            
            -- MUSIC BUTTONS AND SLIDERS
            
           ,[   -- Music Start
                ctButton #: Button False "Start Music"
                , ctScreenRect #: Rectangle 10 80 150 50
            ]
           ,[   -- Music Start
                ctButton #: Button False "Stop Music"
                , ctScreenRect #: Rectangle 180 80 150 50
            ]
           ,[   -- Music item
                ctSoundSource #: Music "Sounds/RMN-Music-Pack/OGG/CD 3 - Clash of Wills/3-04 Joyful Ocean.ogg" 1.0 True "Music"
                , ctPlayCmd #: Stop
            ]
           ,[   -- Art Credits
                ctText #: "Music: Joyful Ocean by Jasprelao"
                , ctScreenRect #: Rectangle 350 80 150 50
            ]

            -- VOLUME SLIDERS 
           ,[   -- slider sounds
                ctSlider #: Slider 1.0 0.75
                , ctScreenRect #: Rectangle 10 160 150 25
            ]
           ,[   -- slider music
                ctSlider #: Slider 1.0 0.75
                , ctScreenRect #: Rectangle 180 160 150 25
            ]
-- CH7-2s
           ,[   -- volume
                ctVolume #: Volume "Sounds" 0.75
            ]
-- CH7-2e
           ,[   -- Slider Text
                ctText #: "Sound Volume"
                , ctScreenRect #: Rectangle 10 200 150 25
            ]
           ,[   -- Art Credits
                ctText #: "Music Volume"
                , ctScreenRect #: Rectangle 180 200 150 25
            ]
            
            
        ]
    return res

rotate eGeo = do
    forever $ do 
        updateC eGeo ctOrientation (\u -> (rotU vec3Y 0.02) .*. (rotU vec3X 0.05) .*. u)
        sleepFor (msecT 20)
    return ()

addActionButton hg3d button action = registerCallback hg3d button ctButton (\(Button flag _) -> if flag then action else return ()) 
    
registerSoundButtons hg3d sound1 sound2 sound3 = do
    mapM (\sound -> addActionButton hg3d sound (setC sound ctPlayCmd Play)) [sound1, sound2, sound3]
          
registerMusicButtons hg3d musicStart musicStop music = do
    addActionButton hg3d musicStart (setC music ctPlayCmd Play)
    addActionButton hg3d musicStop (setC music ctPlayCmd Stop)

-- CH7-3s
registerVolumeSliders hg3d sliderSound sliderMusic volume = do
    let sliderVal val = let (Slider _ val') = val in val'
    registerCallback hg3d sliderSound ctSlider (\val -> setC volume ctVolume (Volume "Sounds" (sliderVal val)))
    registerCallback hg3d sliderMusic ctSlider (\val -> setC volume ctVolume (Volume "Music" (sliderVal val)))
-- CH7-3e

gameLogic hg3d = do
    [camera, cube,
     sound1, sound2, sound3,
     musicStart, musicStop, music, _, 
     sliderSound, sliderMusic, volume, _, _] <- createAll hg3d
    registerSoundButtons hg3d sound1 sound2 sound3
    registerMusicButtons hg3d musicStart musicStop music
    registerVolumeSliders hg3d sliderSound sliderMusic volume
    forkIO $ rotate cube
    return ()

main = do
    runGame standardGraphics3DConfig gameLogic (msecT 20)
    return ()


