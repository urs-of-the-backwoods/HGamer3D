module Main where

import HGamer3D.WinEvent

loopEvents = do
  evt <- pollWinEvent
  case evt of
    Just (EvtQuit ts) -> return ()
    _ -> loopEvents

main = do
  success <- initWinEvent [WEV_INIT_EVENTS, WEV_INIT_TIMER, WEV_INIT_VIDEO]
  print success
  win <- openWindow "HGamer3D - WinEvent window creation test" 100 100 500 300 [SDL_WINDOW_SHOWN]
  loopEvents
  freeWinEvent
  return ()
  
  

