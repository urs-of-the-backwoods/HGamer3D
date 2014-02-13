module Main where

import HGamer3D.WinEvent

loopEvents = do
  evt <- loopHGamer3D
  case evt of
    Just (EvtQuit ts) -> return ()
    _ -> loopEvents

main = do
  success <- initHGamer3D [WEV_INIT_EVENTS, WEV_INIT_TIMER, WEV_INIT_VIDEO]
  win <- openWindow "HGamer3D - WinEvent window creation test" 100 100 500 300 [SDL_WINDOW_SHOWN]
  
  print success
  print win
  loopEvents
  exitHGamer3D
  return ()
  
  

