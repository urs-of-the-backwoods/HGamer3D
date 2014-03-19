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
        
intoEvt :: Functor f  => a -> f b -> f a
intoEvt inval f = fmap (const inval) f

-- book: arithmetic window
getAddWindow ws  = do
  
        leftEB <- doubleBoxW ws "WindowAdd/LeftEB" 0.0
        rightEB <- doubleBoxW ws "WindowAdd/RightEB" 0.0
        sumS <- staticTextW ws "WindowAdd/Sum"
        
        let wire = proc _ -> do
              (leftE, leftV) <- leftEB . never -< ()
              (rightE, rightV) <- rightEB . never -< ()
              sumS -< (show (leftV + rightV)) `intoEvt` (leftE `mergeL` rightE)
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
                inEur' <- delay NoEvent -< inEur
                inDol' <- delay NoEvent -< inDol
                (outEur, _) <- eurs  -< inEur'
                (outDol, _) <- dols  -< inDol'
                let inDol = (* rate) `fmap` outEur
                let inEur = (/ rate) `fmap` outDol
              returnA -< ()        

        return wire
        
_nE event = fmap (const ()) event

getCrudWindow ws  = do
  
  
  
        -- state is a list of: ((prename, surname), idn)
        -- selection is a list of id's with either 1 or 0 elements
        
        let idTp = snd
        let pnTp = fst . fst
        let snTp = snd . fst 
        let nameTp tp = (pnTp tp) ++ " " ++ (snTp tp)
        
        -- deletion and creation functions
        let delF selV = filter (\el -> idTp el /= (selV !! 0) ) 
        let addF (p, s, state, idn) = ((p, s), idn) : state
        
        -- update functions for prename, surname        
        let upPreF state idn pre = fmap (\tp -> if idTp tp == idn then ((pre, snTp tp), idn) else tp) state -- update function for prename
        let upSurF state idn sur = fmap (\tp -> if idTp tp  == idn then ((pnTp tp, sur), idn) else tp) state -- update function for prename
        
        -- filter function, apply the filter box
        let filF fstr stateV = filter (\el -> isInfixOf fstr (nameTp el)) stateV
            
        -- apply selection to state
        let appSelF selV = (\el -> if null selV then (el, False) else (el, (idTp el) == (selV !! 0) )) 
        
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
                -- Values have a V, Events have an E at the end
                (stateV, selV, preE, surE) <- delay ([]::[((String, String), Int)], []::[((String, String), Int)], NoEvent, NoEvent) -< (stateV', selV', preE', surE')
                          
                -- buttons and edit fields
                --------------------------
                                              
                -- prename surname field
                (preE'', preV) <- prenameEB -< preE
                (surE'', surV) <- surnameEB -< surE
                                  
                -- filter string 
                (filE, filV) <- filterEB . now -< ""
                                
                -- create button, depending on selV it is either a create or a deselect event!
                (crE, desE) <- (filterE null &&& filterE (\v -> not (null v))) . createB -< selV
                               
                -- delete button, event contains new list
                dlE <- deleteB -< stateV
                       
                -- modification of stateV by events 
                -----------------------------------
                               
                -- creation of an element
                idV <- (hold . accum1E (+)) <|> pure 0 -< fmap (const (1::Int)) crE      -- this one simply counts ids
                let crE' = fmap (addF . (const (preV, surV, stateV, idV))) crE -- create final event, which contains the new list
                  
                -- deletion of an element
                let dlE' = fmap (delF (map idTp selV)) dlE
                    
                -- change of prename or surname in the data
                let preUp' = if not (null selV) then fmap (upPreF stateV (idTp (selV !! 0))) preE'' else NoEvent
                let surUp' = if not (null selV) then fmap (upSurF stateV (idTp (selV !! 0))) surE'' else NoEvent
                    
                -- summary: stateE', stateV'
                let stateE' =  dlE' `mergeL` crE' `mergeL` preUp' `mergeL` surUp'
                stateV' <- hold <|> pure [] -< stateE'

                -- modification of selV by event
                --------------------------------

                let desE' = fmap (const []) desE
                    
                        
                -- handle display listbox element
                ---------------------------------

                -- create filtered input
                let displayV = fmap (appSelF (fmap idTp selV)) (filF filV stateV')
                 
                -- set display elements on events, get event from selection changes
                (selChangeE, selStateV) <- namesLB -< fmap (const displayV) (_nE stateE' `mergeL` _nE filE)

                -- handle selection 
                -------------------

                selV' <- hold <|> pure [] -< desE' `mergeL` (fmap (const selStateV) (_nE stateE' `mergeL` _nE filE `mergeL` _nE selChangeE)  )

                -- check selection and deselection processes for prename, surname and button text
                selectedE <- filterE (\l -> not (null l)) -< fmap (const selV') (_nE stateE' `mergeL` _nE filE `mergeL` _nE selChangeE)
                deselectedE <- filterE null -< fmap (const selV') (_nE stateE' `mergeL` _nE filE `mergeL` _nE selChangeE)

                -- set prename, surname, if new selection
                let preE' = fmap (\[tp] -> pnTp tp) selectedE
                let surE' = fmap (\[tp] -> snTp tp) selectedE

                -- set button text, depending on selection state`mergeL` Event (fmap idTp selStateV)
                createBTxt -< (fmap (const "Deselect") selectedE) `mergeL` (fmap (const "Create") deselectedE)
                           
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
  
  





