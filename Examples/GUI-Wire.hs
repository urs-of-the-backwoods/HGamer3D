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
 
import HGamer3D.APIs.FRP

import Control.Monad.Trans
import Control.Monad.Reader
import Control.Monad.State

import Control.Monad.Identity (Identity)
import Control.Wire
import Prelude hiding ((.), id)
import Text.Printf
import System.Random

import qualified Data.Map as Map
import Control.Wire.Wire
import Data.Time.Clock

import Control.Concurrent
import Data.List
import GHC.Float


-- define the objects of the gameworld, which will be handed
-- from each instance of hte render loop to the next
 
data GameType =  GtNone
                
renderStep :: TimeMS -> (GameWire a b) -> MHGamer3D (Bool,  GameWire a b ) 
renderStep (TimeMS time) wire = do
    (s, wire') <- stepWire wire ((fromIntegral time)/1000.0) undefined
    return (True, wire')

getWidget widget name = do
  subwidget <- findChildGuiElRecursive widget name
  case subwidget of
    Just widgetOk -> do
      return widgetOk
    Nothing -> do
      return widget
  
initialize3DComponents  = do
	-- add media locations, camera, light, GUI
	addResourceLocationGUI "media\\gui\\layout" "Layout"
	addResourceLocationMedia "media\\materials"
	finalizeResourceLocations
	cam <- getCamera
	let pos = Vec3 5.0 5.0 80.0
	positionTo3D cam pos
	let at = Vec3 0.0 0.0 (-300.0)
	cameraLookAt cam at
	let white = Colour 1.0 1.0 1.0 1.0
	setAmbientLight white
	light <- createPointlight white (Vec3 10.0 10.0 20.0)

	-- GUI Code starts here, display hg3d logo
	loadGuiScheme "hg3d.scheme"
	logo <- loadGuiLayoutFromFile "hgamer3d.layout" ""
	addGuiElToDisplay logo
            
        -- book: layout include	
        -- load and display gui graphics elements
	guiwidgets <- loadGuiLayoutFromFile "wiregui.layout" ""
	addGuiElToDisplay guiwidgets
        -- book: end
        
        -- create a colour cube
        cube <- createRainbowCube
	positionTo3D cube (Vec3 5.0 5.0 0.0)
	scaleTo3D cube (Vec3 10.0 10.0 10.0)
        
        let rw = (orientationToW cube) . mkFix (\t q -> let
                                                   rq = rotU (Vec3 1.0 1.0 0.0)  (double2Float t)
                                                   q' = q .*. rq
                                                   in Right q') . (orientationW cube)
        
        return (guiwidgets, rw)
        
-- book: arithmetic window
getAddWindow guiwidgets = do
  
        -- Arithmetic Window
        leftEB <- getWidget guiwidgets "WindowAdd/LeftEB"        
        rightEB <- getWidget guiwidgets "WindowAdd/RightEB"
        sum <- getWidget guiwidgets "WindowAdd/Sum"
        let (lebc, lebs) = floatEditBoxW leftEB
        let (rebc, rebs) = floatEditBoxW rightEB
        let sums = staticTextW sum
        
        let aww1 = sums . mkFix (\t a -> Right (show a)) . ((+) <$> hold (0.0) lebc <*> hold (0.0) rebc)
        let aww2 = lebs . (once . pure 0.0)
        let aww3 = rebs . (once . pure 0.0)
        let addWindowW = empty . aww1 <|> empty . aww2 <|> empty . aww3 <|> pure GtNone :: GameWire GameType GameType
            
        return addWindowW
-- book: end

-- book: counter window
getCntWindow guiwidgets = do
  
        -- Counter Window
        buttonUp <- getWidget guiwidgets "WindowCounter/ButtonUp"        
        buttonDown <- getWidget guiwidgets "WindowCounter/ButtonDown"
        counter <- getWidget guiwidgets "WindowCounter/Counter"
        let bu = buttonW buttonUp
        let bd = buttonW buttonDown
        let cnts = staticTextW counter
        
        let cntw1 = (once . pure "0")
        let cntw2 = accum1 (\x -> (+x)) 0 . ( (bu . pure 1) <|> (bd . pure (-1)) )
        let cntw3 = cnts . (show <$> cntw2) :: GameWire a String
        let cntWindowW = empty . cntw1 <|> empty . cntw3 <|> pure GtNone :: GameWire GameType GameType
            
        return cntWindowW
-- book: end

-- book: currency window
getCurrWindow guiwidgets rate = do
  
        -- Currentcy Window
        euroEB <- getWidget guiwidgets "WindowCurrency/EuroEB"        
        dollarEB <- getWidget guiwidgets "WindowCurrency/DollarEB"
        (eurc, eurs) <- floatEditBoxW' euroEB
        (dolc, dols) <- floatEditBoxW' dollarEB
        
        let currw1 = eurs . (once . pure 0.0)
        let currw2 = dols . (once . pure 0.0)
        
        let currw3 = eurs . ((/) <$> dolc <*> rate)
        let currw4 = dols . ((*) <$> eurc <*> rate)
        
        let currWindowW = empty . currw3 <|> empty . currw4 <|> empty . currw1 <|> empty. currw1  <|> pure GtNone :: GameWire GameType GameType
            
        return currWindowW
-- book: end

-- book: CRUD example
getCrudWindow guiwidgets = do
  
        -- get GUI elements 
        crunames <- getWidget guiwidgets "WindowCrud/Names"        
        cruname <- getWidget guiwidgets "WindowCrud/Name"        
        crusurname <- getWidget guiwidgets "WindowCrud/Surname"
        createButton <- getWidget guiwidgets "WindowCrud/ButtonCreate"
        deleteButton <- getWidget guiwidgets "WindowCrud/ButtonDelete"
        filterWidget <- getWidget guiwidgets "WindowCrud/Filter"

        -- build basic wires for value changes and setters
        let (namesc, namess) = listBoxW crunames
        let (namec, names) = editBoxW cruname 
        let (surnamec, surnames) = editBoxW crusurname 
        let (filterc, filters) = editBoxW filterWidget
        let createb = buttonW createButton
        let deleteb = buttonW deleteButton
        
        -- a wire, which deletes on element, in case index is set
        let deleteW = mkFix (\t (names, index) -> let
                                names' = case index of
                                  Nothing -> names
                                  Just i -> map fst (filter (\(a, b) -> b /= i) (zip names [0..])) 
                                index' = Nothing
                                in  Right (names', index') )
  
        -- a wire, which creates a new element
        let createW = mkFix (\t namelist ->  Right $ namelist ++ ["Entry, New"]) 
            
        -- a wire, which filters the elements, depending on the "filterc" wire, the content of the filter GUI widget, arrow notation
        let filterW = proc (namelist, index) -> do
              fstr <- hold "" filterc -< undefined
              let namelist' = if (length fstr) > 0 then filter (isInfixOf (fstr::String)) namelist else namelist
              let index' = case index of
                    Nothing -> Nothing
                    Just i -> (if (length rlist) > 0 then Just (rlist !! 0) else Nothing) where
                      rlist = findIndices (== (namelist !! i)) namelist'
              returnA -< (namelist', index')
      
        -- a wire, which selects one element, based on the "namesc" wire, arrow notation
        let selectW = proc (namelist, index) -> do 
              sels' <- (\a -> Just a) <$> namesc <|> id -< Nothing
              let index' = case sels' of
                    Just sels -> if (length sels) > 0 then let 
                      ilist = findIndices (== (sels !! 0)) namelist
                      rvalue = if (length ilist) > 0 then Just (ilist !! 0) else Nothing
                      in rvalue          
                      else Nothing
                    Nothing -> index
              returnA -< (namelist, index')
  
        -- a wire, which takes the surname from the entry
        let getSurnameW = mkFix (\t (names, index) -> case index of 
                           Just i -> Right $ takeWhile (/= ',') (names !! i)
                           Nothing -> Right "")
                
        -- a wire, which takes the name from the entry
        let getNameW = mkFix (\t (names, index) -> case index of 
                           Just i -> Right ( drop 2 $ dropWhile (/= ',') (names !! i))
                           Nothing -> Right "")
                
        -- a wire which modifies the namelist, in case name or surname is changed, arrow notation
        let changeW = proc (namelist, index) -> do
            nametn <- ((\a -> Just a) <$> namec) <|> id -< Nothing
            surnametn <- ((\a -> Just a) <$> surnamec) <|> id -< Nothing
            namet <- getNameW -< (namelist, index)
            surnamet <- getSurnameW -< (namelist, index)
        
            let nametf = case nametn of 
                  Just n -> n
                  Nothing -> namet
            let surnametf = case surnametn of 
                  Just n -> n
                  Nothing -> surnamet
      
            let entrytext =  surnametf ++ ", " ++ nametf
            let namelist' = case index of
                  Just i -> map (\(a, b)-> if b == i then entrytext else a) (zip namelist [0..])
                  Nothing -> namelist
            returnA -< namelist'
            
        -- the main window wire, arrow notation
        let crudWindowW = proc _ -> do
              rec   
                (namelist, index) <- delay ([], Nothing) -< (namelistNew, indexNew)
          
                hold "" (filters . once) -< ""
                hold "" (names . once)  -< ""
                hold "" (surnames . once) -< ""
        
                (namelist', index') <- selectW -< (namelist, index)
                                     
                namelist'' <- changeW -< (namelist', index')
                (filterlist, filterindex) <- filterW -< (namelist'', index') 
                hold [] (namess . changed)  -< filterlist
                hold [] (names . changed . getNameW) -< (filterlist, filterindex)
                hold [] (surnames . changed . getSurnameW) -< (filterlist, filterindex)
            
                let deleteindex = case filterindex of
                      Nothing -> Nothing
                      Just i -> index'

                (namelist''', index'') <- deleteW . deleteb <|> id -< (namelist'', deleteindex)
                namelist'''' <- createW . createb <|> id -< namelist'''
        
                let (namelistNew, indexNew) = (namelist'''', index'')
              
              returnA -< GtNone

        return crudWindowW
-- book: end      

_bToStrW = mkFix (\t a -> if a then (Right "True") else (Right "False"))

-- book: menue window code
-- create the menue on the left side, with the radio button, to select the widgets
getMenueWindow guiwidgets = do

        -- get window GUI elements for all widgets
        addWindow <- getWidget guiwidgets "WindowAdd"        
        counterWindow <- getWidget guiwidgets "WindowCounter"        
        currWindow <- getWidget guiwidgets "WindowCurrency"        
        crudWindow <- getWidget guiwidgets "WindowCrud"        
        
        -- get radio button GUI elements for the selection buttons
        menueAdd <- getWidget guiwidgets "Menue/AddWindow"
        menueCnt <- getWidget guiwidgets "Menue/CntWindow"
        menueCurr <- getWidget guiwidgets "Menue/CurrWindow"
        menueCrud <- getWidget guiwidgets "Menue/CrudWindow"
       
        -- create the FRP wires for "changed value" and "set" functionality for each radio button
        let (addc, adds) = radioButtonW menueAdd
        let (cntc, cnts) = radioButtonW menueCnt
        let (currc, currs) = radioButtonW menueCurr
        let (crudc, cruds) = radioButtonW menueCrud
        
        -- create the FRP wires, which set visibility of the windows, depending on radio button state
        let awis = guiSetPropW addWindow "Visible" . _bToStrW . (addc <|> once . pure False)
        let cwis = guiSetPropW counterWindow "Visible" . _bToStrW . (cntc <|> once . pure False)
        let uwis = guiSetPropW currWindow "Visible" . _bToStrW . (currc <|> once . pure False)
        let cruds = guiSetPropW crudWindow "Visible" . _bToStrW . (crudc <|> once . pure False)
            
        -- create the wire, which is used at the start to select one of the widgets
        let start = adds . once . pure True
        
        -- create one wire, which runs all the wires at each step
        let menueWindowW = empty . awis <|> empty . cwis <|> empty . uwis <|> empty . cruds <|> empty . start <|> pure GtNone :: GameWire GameType GameType
        
        return menueWindowW
-- book: end

initAllWindows guiwidgets = do
  fw1 <- getAddWindow guiwidgets
  fw2 <- getCurrWindow guiwidgets 1.2
  fw3 <- getCntWindow guiwidgets
  fw4 <- getMenueWindow guiwidgets
  fw5 <- getCrudWindow guiwidgets
  let fw = empty . fw1 <|> empty . fw2 <|> empty . fw3 <|> empty . fw4 <|> empty . fw5 <|> pure GtNone 
  return fw
  
  
main = do
  hg <- initHGamer3D "HGamer3D - GUI Wire Example" 
  ((guiwidgets, rw), hg2) <- runMHGamer3D hg initialize3DComponents
  (fw, hg3) <- runMHGamer3D hg2 (initAllWindows guiwidgets)
  let fw' = empty . fw <|> empty . rw <|> pure GtNone
  (x, hg4) <- runMHGamer3D hg2 (renderLoop 40 fw' renderStep) 
  return ()
  
  





