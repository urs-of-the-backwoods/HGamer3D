-- This source file is part of HGamer3D
-- (A project to enable 3D game development in Haskell)
-- For the latest info, see http://www.althainz.de/HGamer3D.html
--
-- (c) 2014 Peter Althainz
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- You may obtain a copy of the License at
--
--     http://www.apache.org/licenses/LICENSE-2.0
--
-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS,
-- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
-- See the License for the specific language governing permissions and
-- limitations under the License.

-- HGamer3D Main Module
--

-- | HGamer3D - A game engine for the Haskell Programmer
module HGamer3D 

(
  HG3DEvent (..),
  
  module HGamer3D.Graphics3D,
  module HGamer3D.WinEvent,
  module HGamer3D.GUI,

  loopHGamer3D,
  initHGamer3D,
  exitHGamer3D
  
  ) where

  import HGamer3D.Graphics3D hiding (initHGamer3D, loopHGamer3D, exitHGamer3D)
  import HGamer3D.WinEvent hiding (initHGamer3D, loopHGamer3D, exitHGamer3D)
  import HGamer3D.GUI
  import qualified System.Info as SI

  data HG3DEvent = EventWindow SDLEvent | EventGUI [GUIEvent]

  exitHGamer3D :: Graphics3DSystem -> GUISystem -> IO ()
  exitHGamer3D g3ds guis = do 
     exitGraphics3D g3ds

  loopHGamer3D :: Graphics3DSystem -> GUISystem -> IO (Maybe HG3DEvent, Bool)
  loopHGamer3D g3ds guis = do

        renderOneFrame g3ds

        -- this one is quite tricky, on Linux we need to call the message loop in addition to WinEvent!
        if SI.os /= "mingw32" then graphics3DPumpWindowMessages else return ()
        i <- checkQuitReceived
        evt <- pollWinEvent
        
        case evt of
           Just sdlEvt -> do
                             injectWinEventToGUI guis sdlEvt  -- inject event into gui
                             return (Just (EventWindow sdlEvt), (i == 1) )
           _ -> do
                  gevts <- pollGUIEvents guis
                  if length gevts > 0 then
                     return (Just (EventGUI gevts), (i == 1) )
                     else
                        return (Nothing, (i == 1) )


  initHGamer3D :: String -- ^ Window Title
                  -> Bool -- ^ Flag show config dialogue
                  -> Bool -- ^ Flag logging enabled
                  -> Bool -- ^ show Graphics Cursor
                  -> IO (Graphics3DSystem, GUISystem, Camera, Viewport)
  initHGamer3D windowTitle fConfigDialogue fLog fGraphicsCursor = do
        success <- initWinEvent [WEV_INIT_EVENTS, WEV_INIT_TIMER, WEV_INIT_VIDEO]
        (g3ds, camera, viewport, window) <- initGraphics3D windowTitle "DefaultSceneManager" fConfigDialogue fLog
        win <- attachToWindow window
        guis <- initGUI fLog fGraphicsCursor
        return (g3ds, guis, camera, viewport)
