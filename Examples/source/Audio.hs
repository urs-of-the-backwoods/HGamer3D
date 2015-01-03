module Main where

import qualified Data.Map as M

import HGamer3D.Data
import HGamer3D.Common
import HGamer3D.Audio

setup = do

  world <- forkAudioWorld (msecT 50)

  let mSlot1 = musicSlot "RMN-Music-Pack/OGG/CD 1 - Journey Begins/1-05 Port Town.ogg"
  let mSlot2 = soundSlot "inventory_sound_effects/metal-clash.wav"

  let audioSource = M.fromList [("port town", mSlot1), ("metal clash", mSlot2)]

  audioE <- newE [CTASr #: audioSource]
  addToWorld world audioE

  return audioE


main = do
     audioE <- setup
     sleepFor (secT 1)
     sendCmd audioE (PlayAudio "port town")
     sleepFor (secT 600)

{-


sendCmd audioE ("Hi Hellow")                    -- wrong cmd type, does nothing, should not crash 
sendCmd audioE (PlayAudio "port town")
sendCmd audioE (PlayAudio "metal clash")
sendCmd audioE (StopAudio "port town")

-}
