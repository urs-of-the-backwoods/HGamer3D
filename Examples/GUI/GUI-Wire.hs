{-# LANGUAGE Arrows #-}


-- This source file is part of HGamer3D
-- (A project to enable 3D game development in Haskell)
-- For the latest info, see http://www.althainz.de/HGamer3D.html
--
-- (c) 2011 Peter Althainz
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


-- GUI-Wire.hs
-- example, to create a basic running HGamer3D Framework with Wire semantics
-- build on top of netwire: http://hackage.haskell.org/package/netwire

module Main where
 
import HGamer3D
import HGamer3D.Wire

import Control.Monad.Trans
import Control.Monad.Reader
import Control.Monad.State

import Control.Monad.Identity (Identity)
import Control.Wire
import Control.Wire.Unsafe.Event
import Prelude hiding ((.), id)
import Text.Printf
import System.Random

import qualified Data.Map as Map
import Data.Time.Clock

import Control.Concurrent
import Data.List
import Data.List.Split
import Data.Maybe
import GHC.Float
import Data.Tuple.Select

import Control.Exception

getWidget widget name = do
  subwidget <- findChildGuiElRecursive widget name
  case subwidget of
    Just widgetOk -> do
      return widgetOk
    Nothing -> do
      return widget
  
initialize3DComponents  ws cam = do
        let g3ds = wsG3d ws
        let guis = wsGui ws
            
	let pos = Vec3 5.0 5.0 80.0
	positionTo3D cam pos
	let at = Vec3 0.0 0.0 (-300.0)
	cameraLookAt cam at
	let white = Colour 1.0 1.0 1.0 1.0
	setAmbientLight g3ds white
	light <- pointLight g3ds white (Vec3 10.0 10.0 20.0)

	-- GUI Code starts here, display hg3d logo
	loadGuiScheme guis "hg3d.scheme"
        loadGuiLayout ws "hgamer3d.layout"
        loadGuiLayout ws "wiregui.layout"
        
	-- create a shiny blue cube
        let cube = cubeMesh
        cube <- object3DFromMesh g3ds cube (Just (ResourceMaterial "Colours/Blue") ) False
        positionTo3D cube (Vec3 5.0 5.0 0.0)
        scaleTo3D cube (Vec3 0.2 0.2 0.2)
        rE <- rotEntity "cubeE" cube unitU
        let rw = rotate rE (Vec3 1.0 1.0 0.0) 1.0
        
        return (rw, rE)
        
-- book: arithmetic window
getAddWindow ws  = do
  
        -- Arithmetic Window
        leb <- doubleBoxW ws "WindowAdd/LeftEB" 0.0
        reb <- doubleBoxW ws "WindowAdd/RightEB" 0.0
        sums <- staticTextW ws "WindowAdd/Sum"
            
        let wire = proc _ -> do
              valL <- hold . leb . (now &&& now ) -< 0.0
              valR <- hold . reb . (now &&& now ) -< 0.0
              let s = show (valL + valR)
              sums . (never &&& id) -< Event s
              returnA -< ()
        
        return wire

printCheck :: String -> GameWire (Event a) (Event a)
printCheck marker = mkGen_ (\a -> case a of 
                        Event _ -> do
                                   print $ marker ++ ": event received"
                                   return (Right a)
                        _ -> do 
                          return (Right a))
  
getCntWindow ws  = do
  
        -- Counter Window
        bu <- buttonW ws "WindowCounter/ButtonUp"
        bd <- buttonW ws "WindowCounter/ButtonDown"
        cnts <- staticTextW ws "WindowCounter/Counter"
        
        let wire = proc _ -> do
              ae <- bu -< 1
              se <- bd -< (-1)
              sum <- accumE (\a b -> a + b) 0.0 -< mergeL ae se
              let ssum = fmap show sum
              cnts . (never &&& id) -< ssum
              returnA -< ()
            
        return wire
        
getCurrWindow ws rate = do
  
        -- Currentcy Window
        eurs <- doubleBoxW ws "WindowCurrency/EuroEB" 0.0 
        dols <- doubleBoxW ws "WindowCurrency/DollarEB" 0.0
        
        let wire = proc _ -> do
              rec
                inEur' <- delay NoEvent -< inEur
                inDol' <- delay NoEvent -< inDol
                outEur <- eurs . (never &&& id) -< inEur'
                outDol <- dols . (never &&& id) -< inDol'
                let inDol = fmap (* rate) outEur
                let inEur = fmap (/ rate) outDol
              returnA -< ()        

        return wire
        
_nE event = fmap (const ()) event

getCrudWindow ws  = do
  
        -- get GUI elements 
        names <- listBoxW ws "WindowCrud/Names"
        prename <- editBoxW ws "WindowCrud/Name"      
        surname <- editBoxW ws "WindowCrud/Surname"
        create <- buttonW ws "WindowCrud/ButtonCreate"
        createText <- guiPropertyW ws "WindowCrud/ButtonCreate" "Text"
        delete <- buttonW ws "WindowCrud/ButtonDelete"
        filterstr <- editBoxW ws "WindowCrud/Filter"

        -- state is a list of: (entrytext, selected, id)
        -- functions to process this type of state
        
        let addNW = (iterateE 0 . (arr . fmap) (const (+ 1))) 
        let addF (p, s, l, n) = if (length p > 0) && (length s > 0) then ((p ++ " " ++ s, False, n) : l) else l 
        let delF l = filter (\(e, s, n) -> not s) l 
        let filF l rex = filter (\(e, s, n) -> (isInfixOf rex e) ) l
        let desF state = fmap (\(e, s, n) -> (e, False, n)) state
        let selF state = Data.List.find (\(e, s, n) -> s) state
        let upPreF state n pre = fmap (\(e, s, n') -> if n == n' then 
                                                        (pre ++ " " ++ ((splitOn " " e) !! 1), s, n) 
                                                      else (e, s, n') ) state
        let upSurF state n sur = fmap (\(e, s, n') -> if n == n' then
                                                        (((splitOn " " e) !! 0) ++ " " ++ sur, s, n) 
                                                      else (e, s, n') ) state
            
        let mergeState vOri vChange = let
              appChanges = \(e', s', n') (e, s, n) -> if n == n' then (e, s', n) else (e, s, n)
              onApp c l = fmap (appChanges c) l
              in foldr onApp vOri vChange
              
                 
        -- the main wire
        ----------------
            
        let wire = proc _ -> do
              rec
                -- Values have a V, Events have an E at the end
                (selV, stateV, preE, surE) <- delay (Nothing, []::[(String, Bool, Int)], NoEvent, NoEvent) -< (selV', stateV'', preE', surE')
                         
                                 
                -- events happening, values created
                                 
                -- prename surname field
                preE'' <- prename . (now &&& id) -< preE
                surE'' <- surname . (now &&& id) -< surE
                preV <- hold -< (preE'' `mergeL` preE)
                surV <- hold -< (surE'' `mergeL` surE)
                -- filter string 
                filE <- filterstr . (now &&& now) -< ""
                filV <- hold -< filE
                let filE' = fmap (filF stateV) filE
                
                
                -- create button, depending on selV it is either a create or a deselect event!
                (crE, desE) <- (filterE isNothing &&& filterE isJust) . create -< selV
                n <- (hold . addNW) <|> pure 0 -< crE      -- this one simply counts ids
                let crE' = fmap (addF . (const (preV, surV, stateV, n))) crE
                -- delete button
                dlE' <- arr (fmap delF) . delete -< stateV
                -- deselection
                let desE' = fmap (const (desF stateV)) desE
                -- prename surname change
                let selUp' = if isJust selV then
                       let selN = (\(e, s, n) -> n) $ fromJust selV
                           preUp' = fmap (upPreF stateV selN) preE''
                           surUp' = fmap (upSurF stateV selN) surE''
                           in preUp' `mergeL` surUp'
                       else
                           NoEvent
                   
                     
                -- main element, the displayed list                       
                let inE = fmap ((flip filF) filV) (dlE' `mergeL` crE' `mergeL` filE' `mergeL` desE' `mergeL` selUp')
                listCE <- names . (id &&& id) -< inE
                visStateV <- hold -< listCE

                -- selection check
                selE <- arr (fmap fromJust) . filterE isJust . arr (fmap selF) -< listCE

                -- main state handling
                let stateV' = event stateV id (crE' `mergeL` dlE' `mergeL` desE')
                let stateV'' = mergeState stateV' visStateV
                        
                -- get selection n
                let selV' = selF stateV''
                let selES = fmap (\(e, s, n) -> e) selE
                let preE' = fmap (\x -> (splitOn " " x) !! 0) selES
                let surE' = fmap (\x -> (splitOn " " x) !! 1) selES

                -- set button text
                createText . (id &&& id) -< fmap (const "Deselect") selE
                -- handle deselection
                createText . (id &&& id) -< fmap (const "Create") ((_nE desE) `mergeL` (_nE dlE'))

                           
              returnA -< ()
              
        return wire
            
getMenueWindow ws  = do

        -- create the FRP wires for "changed value" and "set" functionality for each radio button
        bAdd <- radioButtonW ws "Menue/AddWindow"
        bCnt <- radioButtonW ws "Menue/CntWindow"
        bCurr <- radioButtonW ws "Menue/CurrWindow"
        bCrud <- radioButtonW ws "Menue/CrudWindow"
        
        -- create the FRP wires, which set visibility of the windows, depending on radio button state
        vAdd <- guiPropertyW ws "WindowAdd" "Visible" 
        vCnt <- guiPropertyW ws "WindowCounter" "Visible" 
        vCurr <- guiPropertyW ws "WindowCurrency" "Visible" 
        vCrud <- guiPropertyW ws "WindowCrud" "Visible" 
        
        let nn = now &&& now
        let b2st = (arr . fmap) (\b -> if b then "True" else "False")
        let nb = now &&& b2st

        -- create the menu wire
        let menueWindowW = proc _ -> do
              vAdd . nb . bAdd . nn -< True 
              vCnt . nb . bCnt . nn -< False
              vCurr . nb . bCurr . nn -< False
              vCrud . nb . bCrud . nn -< False
              returnA -< ()
              
        return menueWindowW
              
initAllWindows ws rw = do
  fw1 <- getAddWindow ws
  fw2 <- getMenueWindow ws
  fw3 <- getCntWindow ws 
  fw4 <- getCurrWindow ws 1.3738
  fw5 <- getCrudWindow ws
  let fw = proc _ -> do
        fw1 -< ()
        fw2 -< ()
        fw3 -< ()
        fw4 -< ()
        fw5 -< ()
        rw -< Event ()
        returnA -< ()
  return fw
  
  
  
renderLoop ws s w cmd = do
  (ds, s') <- stepSession s
  (cmd', w') <- stepWire w ds cmd
  qFlag <- HGamer3D.Wire.loopHGamer3D ws
  if qFlag then return () else renderLoop ws s' w' cmd'
  
main = do
  (ws, camera, viewport) <- HGamer3D.Wire.initHGamer3D "HGamer3D - GUI Wire Example" True False True
  (rw, rE) <- initialize3DComponents ws camera
  fw <- initAllWindows ws rw
  
  let startCmd = (CmdAddEntity rE)
    
  let locSysW = createSystem locationSystem
  let oriSysW = createSystem orientationSystem
  let velSysW = createSystem velocitySystem
  let fw' = (proc cmd -> do
        fw -< ()
        cmd' <- cmdFifo . locSysW . velSysW . oriSysW -< cmd
        returnA -< cmd') :: GameWire (Event Command) (Event Command)
       
  renderLoop ws clockSession_ fw' (Right (Event startCmd))
  return ()
  
  





