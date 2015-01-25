module Main where

-- remark this example shows WinEvent in isolation, which is not very useful
-- see BlueCube example for basic HGamer3D window creation

import HGamer3D.Graphics3D.WinEvent

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
  
  

