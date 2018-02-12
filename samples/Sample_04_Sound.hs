{-# LANGUAGE OverloadedStrings #-}

module Sample_04_Sound where

import HGamer3D

import qualified Data.Text as T
import Control.Concurrent
import Control.Monad
import SampleRunner

createAll hg3d = do

    -- create all elements
    res <- mapM (newE hg3d) [


           [   -- metal-clash button and sound
                ctButton #: Button False "metal clash"
                , ctScreenRect #: ScreenRect 60 110 150 50
                , ctSoundSource #: SoundSource Sound "Sounds/inventory_sound_effects/metal-clash.wav" False 1.0 "Sounds"
                , ctPlayCmd #: Stop
            ]

            -- HGamer3D website, events, create sound source
           ,[   -- ring-inventory button and sound
                ctButton #: Button False "ring inventory"
                , ctScreenRect #: ScreenRect 230 110 150 50
                , ctSoundSource #: SoundSource Sound "Sounds/inventory_sound_effects/ring_inventory.wav" False 1.0 "Sounds"
                , ctPlayCmd #: Stop
            ]
            -- end of website text
-- CH7-1e
           ,[   -- sell_buy_item button and sound
                ctButton #: Button False "sell buy item"
                , ctScreenRect #: ScreenRect 400 110 150 50
                , ctSoundSource #: SoundSource Sound "Sounds/inventory_sound_effects/sell_buy_item.wav" False 1.0 "Sounds"
                , ctPlayCmd #: Stop
            ]

            -- MUSIC BUTTONS AND SLIDERS

           ,[   -- Music Start
                ctButton #: Button False "Start Music"
                , ctScreenRect #: ScreenRect 60 180 150 50
            ]
           ,[   -- Music Start
                ctButton #: Button False "Stop Music"
                , ctScreenRect #: ScreenRect 230 180 150 50
            ]
           ,[   -- Music item
                ctSoundSource #: SoundSource Music "Sounds/RMN-Music-Pack/OGG/CD 3 - Clash of Wills/3-04 Joyful Ocean.ogg" True 1.0 "Music"
                , ctPlayCmd #: Stop
            ]
           ,[   -- Art Credits
                ctStaticText #: "Music: Joyful Ocean by Jasprelao"
                , ctScreenRect #: ScreenRect 400 180 150 50
            ]

            -- VOLUME SLIDERS 
           ,[   -- slider sounds
                ctSlider #: Slider 1.0 0.75
                , ctScreenRect #: ScreenRect 60 260 150 25
            ]
           ,[   -- slider music
                ctSlider #: Slider 1.0 0.75
                , ctScreenRect #: ScreenRect 230 260 150 25
            ]
-- CH7-2s
           ,[   -- volume
                ctVolume #: Volume "Sounds" 0.75
            ]
-- CH7-2e
           ,[   -- Slider Text
                ctStaticText #: "Sound Volume"
                , ctScreenRect #: ScreenRect 60 300 150 25
            ]
           ,[   -- Art Credits
                ctStaticText #: "Music Volume"
                , ctScreenRect #: ScreenRect 230 300 150 25
            ]
        ]
    return res

addActionButton hg3d button action = registerCallback hg3d button ctButton (\(Button flag _) -> if flag then action else return ()) 

-- HGamer3D website, events, send play event
registerSoundButtons hg3d sound1 sound2 sound3 = do
    mapM (\sound -> addActionButton hg3d sound (setC sound ctPlayCmd Play)) [sound1, sound2, sound3]
-- end of website text

registerMusicButtons hg3d musicStart musicStop music = do
    addActionButton hg3d musicStart (setC music ctPlayCmd Play)
    addActionButton hg3d musicStop (setC music ctPlayCmd Stop)

-- CH7-3s
registerVolumeSliders hg3d sliderSound sliderMusic volume = do
    let sliderVal val = let (Slider _ val') = val in val'
    registerCallback hg3d sliderSound ctSlider (\val -> setC volume ctVolume (Volume "Sounds" (sliderVal val)))
    registerCallback hg3d sliderMusic ctSlider (\val -> setC volume ctVolume (Volume "Music" (sliderVal val)))
-- CH7-3e

creator hg3d = do
    elist <- createAll hg3d
    let [ sound1, sound2, sound3,
          musicStart, musicStop, music, _, 
          sliderSound, sliderMusic, volume, _, _] = elist
    registerSoundButtons hg3d sound1 sound2 sound3
    registerMusicButtons hg3d musicStart musicStop music
    registerVolumeSliders hg3d sliderSound sliderMusic volume
    return elist

destructor elist = do
  let [   sound1, sound2, sound3,
          musicStart, musicStop, music, t1, 
          sliderSound, sliderMusic, volume, t2, t3] = elist
  mapM delE [sliderSound, sliderMusic, musicStart, musicStop, sound1, sound2, sound3]
  mapM delE [volume, music, t1, t2, t3]
  return ()

sampleRunner hg3d = SampleRunner (return ()) (do
                                    state <- creator hg3d
                                    return (SampleRunner (destructor state) (return emptySampleRunner) ))

