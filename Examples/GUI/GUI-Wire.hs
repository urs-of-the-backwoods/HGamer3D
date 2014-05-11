{-# LANGUAGE Arrows #-}


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


-- GUI-Wire.hs
-- example, to create and use GUI elements efficiently with netwire
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
-- import Data.List.Split
import Data.Maybe
import GHC.Float
-- import Data.Tuple.Select

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
        
-- define some operators for events and values
_nE evt = fmap (const ()) evt

(|#) :: Event a -> Event a -> Event a
evt1 |# evt2 = evt1 `mergeL` evt2

(|!) :: Event a -> Event b -> Event ()
evt1 |! evt2 = _nE evt1 `mergeL` _nE evt2

(<#) :: Event a -> b -> Event b
evt <# val = fmap (const val) evt


-- book: arithmetic window
getAddWindow ws  = do
  
        leftEB <- doubleBoxW ws "WindowAdd/LeftEB" 0.0
        rightEB <- doubleBoxW ws "WindowAdd/RightEB" 0.0
        sumS <- staticTextW ws "WindowAdd/Sum"
        
        let wire = proc _ -> do
              (leftE, leftV) <- leftEB . never -< ()
              (rightE, rightV) <- rightEB . never -< ()
              sumS -< (leftE |# rightE) <# (show (leftV + rightV)) 
              returnA -< ()
        
        return wire


getCntWindow ws  = do
  
        -- Counter Window
        buttonUp <- buttonW ws "WindowCounter/ButtonUp"
        buttonDown <- buttonW ws "WindowCounter/ButtonDown"
        cnts <- staticTextW ws "WindowCounter/Counter"
        
        let wire = proc _ -> do
              addE <- buttonUp -< 1
              subE <- buttonDown -< (-1)
              sumE <- accumE (+) 0 -< mergeL addE subE
              cnts -< show `fmap` sumE
              returnA -< ()        
            
        return wire
        
getCurrWindow ws rate = do
  
        -- Currentcy Window
        eurs <- doubleBoxW ws "WindowCurrency/EuroEB" 0.0 
        dols <- doubleBoxW ws "WindowCurrency/DollarEB" 0.0
        
        let wire = proc _ -> do
              rec
                (outEur, _) <- eurs . delay NoEvent -< inEur
                (outDol, _) <- dols . delay NoEvent -< inDol
                let inDol = (* rate) `fmap` outEur
                let inEur = (/ rate) `fmap` outDol
              returnA -< ()        

        return wire
        
getCrudWindow ws  = do
  
        -- stateV is a list of: ((prename, surname), idn)
        -- selV has the same format but only contains the selected items
        
        let idTp = snd
        let pnTp = fst . fst
        let snTp = snd . fst 
        let nameTp tp = (pnTp tp) ++ " " ++ (snTp tp)
        
        -- get GUI elements 
        namesLB <- listBoxW ws "WindowCrud/Names" nameTp []
        prenameEB <- editBoxW ws "WindowCrud/Name" ""    
        surnameEB <- editBoxW ws "WindowCrud/Surname" ""
        createB <- buttonW ws "WindowCrud/ButtonCreate"
        createBTxt <- guiSetPropertyW ws "WindowCrud/ButtonCreate" "Text"
        deleteB <- buttonW ws "WindowCrud/ButtonDelete"
        filterEB <- editBoxW ws "WindowCrud/Filter" ""

             
                 
        -- the main wire
        ----------------
            
        let wire = proc _ -> do
              rec

                -- setup main events, those events drive all computations
                ---------------------------------------------------------

                -- need delay in recursive definition
                (stateInE, selInE) <- delay (Event [], Event []) -< (stateInE', selInE')

                -- all elements, wired with in and out and value
                (preOutE, preV) <- prenameEB . delay (Event "") -< preInE
                (surOutE, surV) <- surnameEB . delay (Event "") -< surInE
                (filOutE, filV) <- filterEB . now -< ""
                (selNamesOutE, selNamesV) <- namesLB . delay (Event []) -< namesInE
                createBTxt . delay (Event "Create") -< creBInE
                
                -- additional elements, which only create events
                creBE <- createB -< ()
                delBE <- deleteB -< ()

                -- values, computed from events, main state info of program
                stateV <- hold -< stateInE
                selV <- hold -< selInE
                
                -- specific computed events
                selectedE <- filterE (not . null) -< selInE <# selV          -- selection appeared
                deselectedE <- filterE null -< selInE <# selV                        -- deselection appeard
                (crE, desE) <- (filterE null &&& filterE (not . null)) -< creBE <# selV      -- create button is either create or deselect evt
                preOutSelE <- filterE (not . null) -< preOutE <# selV        -- only set pre field, when selection
                surOutSelE <- filterE (not . null) -< surOutE <# selV        -- only set sur field, when selection

                -- value, computed from events        
                idV <- (hold . accum1E (+)) <|> pure 0 -< fmap (const (1::Int)) crE      -- counts ids upon creation

                              
                -- now the logic, for each event
                --------------------------------

                -- prename surname fields get set upon new selection
                let preInE = fmap (\[tp] -> pnTp tp) selectedE 
                let surInE = fmap (\[tp] -> snTp tp) selectedE
              
                -- main state value, change the list of items
                let stateInE' =    (crE <# (((preV, surV), idV) : stateV)  )      -- creation of element
                                |# (delBE <# if null selV then stateV else filter (\el -> idTp el /= idTp (selV !! 0) ) stateV) -- deletion of element
                                |# (preOutSelE <# (
                                     let updatePre idx tp = if idTp tp == idTp idx then ((preV, snTp tp), idTp tp) else tp
                                     in (fmap (updatePre (selV !! 0)) stateV ) ))           -- pre changed
                                |# (surOutSelE <# (
                                     let updateSur idx tp = if idTp tp == idTp (selV !! 0) then ((pnTp tp, surV), idTp tp) else tp
                                     in (fmap (updateSur (selV !! 0)) stateV ) ))           -- sur changed

                -- selection state value, change list of selected items
                let selInE' =      (desE <# []) 
                                |# (delBE <# [])
                                |# (selNamesOutE <# selNamesV)
                                |# (filOutE <# (filter (isInfixOf filV . nameTp) selNamesV) )

                -- names dialog box, filter by search string and re-apply selection
                let namesInE =  (stateInE |! selInE |! filOutE) <# ( 
                         let reSelect el = (el, if null selV then False else (idTp el) == idTp (selV !! 0))
                             filterF = filter ( (isInfixOf filV) . nameTp )
                         in fmap reSelect (filterF stateV) )

                -- create button text, in value
                let creBInE  =     (selectedE <# "Deselect") 
                                |# (deselectedE <# "Create")

              returnA -< ()
              
        return wire
            
getMenueWindow ws  = do

        -- create the FRP wires for "changed value" and "set" functionality for each radio button
        bAdd <- radioButtonW ws "Menue/AddWindow" True
        bCnt <- radioButtonW ws "Menue/CntWindow" False
        bCurr <- radioButtonW ws "Menue/CurrWindow" False
        bCrud <- radioButtonW ws "Menue/CrudWindow" False
        
        -- create the FRP wires, which set visibility of the windows, depending on radio button state
        vAdd <- guiSetPropertyW ws "WindowAdd" "Visible"
        vCnt <- guiSetPropertyW ws "WindowCounter" "Visible"
        vCurr <- guiSetPropertyW ws "WindowCurrency" "Visible"
        vCrud <- guiSetPropertyW ws "WindowCrud" "Visible"
        
        let b2st = \b ->if b then "True" else "False"

        -- create the menu wire
        let menueWindowW = proc _ -> do 
              (addE, _) <- bAdd . never -< ()
              (cntE, _) <- bCnt . never -< ()
              (currE, _) <- bCurr . never -< ()
              (crudE, _) <- bCrud . never -< ()
              
              vAdd . (id &> now . pure "True" ) -< b2st `fmap` addE
              vCnt . (id &> now . pure "Fals" ) -< b2st `fmap` cntE
              vCurr . (id &> now . pure "Fals" ) -< b2st `fmap` currE
              vCrud . (id &> now . pure "Fals" ) -< b2st `fmap` crudE 
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
  
  





