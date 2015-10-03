{-# LANGUAGE OverloadedStrings #-}

module Main where

import HGamer3D

import qualified Data.Text as T
import Control.Concurrent
import Control.Monad

createAll = do
    -- graphics system creation
    eG3D <- newE [
        ctGraphics3DConfig #: standardGraphics3DConfig,
        ctGraphics3DCommand #: NoCmd
        ]
    world <- forkGraphics3DWorld (setC eG3D ctGraphics3DCommand Step >> return False) (msecT 20)
    addToWorld world eG3D
    
    -- creator helper
    let creator l = do
        e <- newE l
        addToWorld world e
        return e
        
    -- create all elements
    res <- mapM creator [
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
                ctText #: "metal clash"
                , ctButton #: False
                , ctScreenRect #: Rectangle 10 10 150 50
                , ctSoundSource #: Sound "Sounds/inventory_sound_effects/metal-clash.wav" 1.0 False "Sounds"
                , ctPlayCmd #: Stop
            ]
           ,[   -- ring-inventory button and sound
                ctText #: "ring inventory"
                , ctButton #: False
                , ctScreenRect #: Rectangle 180 10 150 50
                , ctSoundSource #: Sound "Sounds/inventory_sound_effects/ring_inventory.wav" 1.0 False "Sounds"
                , ctPlayCmd #: Stop
            ]
           ,[   -- sell_buy_item button and sound
                ctText #: "sell buy item"
                , ctButton #: False
                , ctScreenRect #: Rectangle 350 10 150 50
                , ctSoundSource #: Sound "Sounds/inventory_sound_effects/sell_buy_item.wav" 1.0 False "Sounds"
                , ctPlayCmd #: Stop
            ]
            
            -- MUSIC BUTTONS AND SLIDERS
            
           ,[   -- Music Start
                ctText #: "Start Music"
                , ctButton #: False
                , ctScreenRect #: Rectangle 10 80 150 50
            ]
           ,[   -- Music Start
                ctText #: "Stop Music"
                , ctButton #: False
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
           ,[   -- volume
                ctVolume #: Volume "Sounds" 0.75
            ]
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
        updateC eGeo ctOrientation (\u -> (rotU vec3Y 0.02) .*. (rotU vec3X 0.005) .*. u)
        sleepFor (msecT 20)
    return ()

addActionButton button action = addListener button ctButton (\_ enew -> 
            if ((enew #! ctButton) == False)
                then action
                else return () )
    
registerSoundButtons sound1 sound2 sound3 = do
    mapM (\sound -> addActionButton sound (setC sound ctPlayCmd Play)) [sound1, sound2, sound3]
          
registerMusicButtons musicStart musicStop music = do
    addActionButton musicStart (setC music ctPlayCmd Play)
    addActionButton musicStop (setC music ctPlayCmd Stop)

registerVolumeSliders sliderSound sliderMusic volume = do
    let sliderVal e = let (Slider _ val) = e #! ctSlider in val
    addListener sliderSound ctSlider (\_ new -> setC volume ctVolume (Volume "Sounds" (sliderVal new)))
    addListener sliderMusic ctSlider (\_ new -> setC volume ctVolume (Volume "Music" (sliderVal new)))
    
main = do
    [camera, cube,
     sound1, sound2, sound3,
     musicStart, musicStop, music, _, 
     sliderSound, sliderMusic, volume, _, _] <- createAll
    registerSoundButtons sound1 sound2 sound3
    registerMusicButtons musicStart musicStop music
    registerVolumeSliders sliderSound sliderMusic volume
    rotate cube
