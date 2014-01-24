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
 
import HGamer3D.WireAPI

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

import HGamer3D.WireAPI

-- define the objects of the gameworld, which will be handed
-- from each instance of hte render loop to the next
 
data GameType = GtO Object3D 
              | GtC Camera
              | GtI Int
              | GtF Float
              | GtB Bool    
              | GtS String
              | GtW (GameWire String String)
              | GtA [GameType]
              | GtM (Map.Map String GameType)
              | GtV Vec3
              | GtG GUIElement
              | GtNone
                
renderStep :: TimeMS -> (GameType, GameWire GameType GameType) -> MHGamer3D (Bool, (GameType, GameWire GameType GameType) )
renderStep (TimeMS time) (s, wire) = do
   (s', wire') <- stepWire wire ((fromIntegral time)/1000.0) s
   return $ case s' of
        Right s'' -> (True, (s'', wire'))
        Left () -> (False, (s, wire'))


getW :: String -> MVar (Map.Map String GameType) -> GameWire a GameType
getW obname map = mkFixM (\t x -> do
                             ob <- liftIO $ getM map obname
                             case ob of
                               GtNone -> return (Left ())
                               _ -> return (Right ob))

rotWire = mkFix (\t vec ->  Right  (vec &+ Vec3 10.0 10.0 10.0))
          

init3D = do
	-- initialize HGamer3D
	hg <- initHGamer3D "HGamer3D - GUI Wire Example" 
--        let map = (fromList [])::GameType
        return hg

--run3D hg sub = do
--	-- run HGamer3D routines, here main 3D program
--	(l, hg2) <- runMHGamer3D hg sub
--	return (l, hg2)
run3D = runMHGamer3D

createM = do
  ref <- newMVar ((Map.fromList [])::Map.Map String GameType)
  return ref
  
getM ref name = do
  map <- readMVar ref
  case Map.lookup name map of
    Just ob -> return ob
    Nothing -> return GtNone
    
putM ref name ob = do
  map <- takeMVar ref
  let nmap = Map.insert name ob map
  putMVar ref nmap
  return ()
  
getWidget widget name = do
  subwidget <- findChildGuiElRecursive widget name
  case subwidget of
    Just widgetOk -> do
      return widgetOk
    Nothing -> do
      return widget
  
initialize3DComponents  map = do
	-- define the 3D program
	-- initialize graphics objects and start rendering loop
	
	-- add media locations
	addResourceLocationGUI "media\\gui\\layout" "Layout"
	addResourceLocationMedia "media\\materials"
	finalizeResourceLocations

	-- camera position
	cam <- getCamera
	let pos = Vec3 5.0 5.0 80.0
	positionTo3D cam pos
	let at = Vec3 0.0 0.0 (-300.0)
	cameraLookAt cam at
        liftIO $ putM map "Camera" (GtC cam)
	
	-- define light
	let white = Colour 1.0 1.0 1.0 1.0
	setAmbientLight white
	light <- createPointlight white (Vec3 10.0 10.0 20.0)

	-- GUI Code starts here, display hg3d logo
	loadGuiScheme "hg3d.scheme"
	logo <- loadGuiLayoutFromFile "hgamer3d.layout" ""
	addGuiElToDisplay logo
            
	-- load and display gui graphics elements
	guiwidgets <- loadGuiLayoutFromFile "wiregui.layout" ""
	addGuiElToDisplay guiwidgets
        
        addWindow <- getWidget guiwidgets "WindowAdd"        
        leftEB <- getWidget guiwidgets "WindowAdd/LeftEB"        
        rightEB <- getWidget guiwidgets "WindowAdd/RightEB"
        sum <- getWidget guiwidgets "WindowAdd/Sum"
        liftIO $ putM map "awi" (GtG addWindow)
        liftIO $ putM map "leb" (GtG leftEB)
        liftIO $ putM map "reb" (GtG rightEB)
        liftIO $ putM map "sum" (GtG sum)
        
        counterWindow <- getWidget guiwidgets "WindowCounter"        
        buttonUp <- getWidget guiwidgets "WindowCounter/ButtonUp"        
        buttonDown <- getWidget guiwidgets "WindowCounter/ButtonDown"
        counter <- getWidget guiwidgets "WindowCounter/Counter"
        liftIO $ putM map "cwi" (GtG counterWindow)
        liftIO $ putM map "buu" (GtG buttonUp)
        liftIO $ putM map "bud" (GtG buttonDown)
        liftIO $ putM map "cnt" (GtG counter)
        
        currWindow <- getWidget guiwidgets "WindowCurrency"        
        euroEB <- getWidget guiwidgets "WindowCurrency/EuroEB"        
        dollarEB <- getWidget guiwidgets "WindowCurrency/DollarEB"
        liftIO $ putM map "uwi" (GtG currWindow)
        liftIO $ putM map "eeb" (GtG euroEB)
        liftIO $ putM map "deb" (GtG dollarEB)
        
        crudWindow <- getWidget guiwidgets "WindowCrud"        
        names <- getWidget guiwidgets "WindowCrud/Names"        
        name <- getWidget guiwidgets "WindowCrud/Name"        
        surname <- getWidget guiwidgets "WindowCrud/Surname"
        createButton <- getWidget guiwidgets "WindowCrud/ButtonCreate"
        deleteButton <- getWidget guiwidgets "WindowCrud/ButtonDelete"
        filterWidget <- getWidget guiwidgets "WindowCrud/Filter"
        liftIO $ putM map "cruwi" (GtG crudWindow)
        liftIO $ putM map "crunames" (GtG names)
        liftIO $ putM map "cruname" (GtG name)
        liftIO $ putM map "crusurname" (GtG surname)
        liftIO $ putM map "crucreate" (GtG createButton)
        liftIO $ putM map "crudelete" (GtG deleteButton)
        liftIO $ putM map "crufilter" (GtG filterWidget)

        menueAdd <- getWidget guiwidgets "Menue/AddWindow"
        menueCnt <- getWidget guiwidgets "Menue/CntWindow"
        menueCurr <- getWidget guiwidgets "Menue/CurrWindow"
        menueCrud <- getWidget guiwidgets "Menue/CrudWindow"
        liftIO $ putM map "mAdd" (GtG menueAdd)
        liftIO $ putM map "mCnt" (GtG menueCnt)
        liftIO $ putM map "mCurr" (GtG menueCurr)
        liftIO $ putM map "mCrud" (GtG menueCrud)
       
	-- create a shiny blue cube
--	cubeBlue <- createCube
--	setObjectMaterial cubeBlue (NamedMaterial "Template/Blue")
--	positionTo3D cubeBlue (Vec3 0.0 0.0 0.0)
--	scaleTo3D cubeBlue (Vec3 0.2 0.2 0.2)
--      liftIO $ putM map "Cube" (GtO cubeBlue)
            
        return ()

_bToStrW = mkFix (\t flag -> Right $ if flag then "True" else "False")

getMenueWindow m = do
  (GtG mAdd) <- getM m "mAdd"
  (GtG mCnt) <- getM m "mCnt"
  (GtG mCurr) <- getM m "mCurr"
  (GtG mCrud) <- getM m "mCrud"
  
  (GtG awi) <- getM m "awi"
  (GtG cwi) <- getM m "cwi"
  (GtG uwi) <- getM m "uwi"
  (GtG cruwi) <- getM m "cruwi"
  
  let (addc, adds) = radioButtonW mAdd
  let (cntc, cnts) = radioButtonW mCnt
  let (currc, currs) = radioButtonW mCurr
  let (crudc, cruds) = radioButtonW mCrud
      
  let awis = guiSetPropW awi "Visible" . _bToStrW . (addc <|> once . pure False)
  let cwis = guiSetPropW cwi "Visible" . _bToStrW . (cntc <|> once . pure False)
  let uwis = guiSetPropW uwi "Visible" . _bToStrW . (currc <|> once . pure False)
  let cruds = guiSetPropW cruwi "Visible" . _bToStrW . (crudc <|> once . pure False)
  let start = adds . once . pure True
  
  let fw = empty . awis <|> empty . cwis <|> empty . uwis <|> empty . cruds <|> empty . start <|> pure GtNone :: GameWire GameType GameType
  return fw
  
getCrudWindow m = do
  (GtG crunames) <- getM m "crunames"
  (GtG cruname) <- getM m "cruname"
  (GtG crusurname) <- getM m "crusurname"
  (GtG crufilter) <- getM m "crufilter"
  (GtG crucreate) <- getM m "crucreate"
  (GtG crudelete) <- getM m "crudelete"
  
  let (namesc, namess) = listBoxW crunames
  let (namec, names) = editBoxW cruname 
  let (surnamec, surnames) = editBoxW crusurname 
  let (filterc, filters) = editBoxW crufilter
  let createb = buttonW crucreate
  let deleteb = buttonW crudelete
      
  let deleteW = mkFix (\t (names, index) -> let
                          names' = case index of
                            Nothing -> names
                            Just i -> map fst (filter (\(a, b) -> b /= i) (zip names [0..])) 
                          index' = Nothing
                          in  Right (names', index') )
  
  let createW = mkFix (\t namelist ->  Right $ namelist ++ ["Entry, New"]) 
                  
  let filterW = proc (namelist, index) -> do
        fstr <- hold "" filterc -< undefined
        let
                                         namelist' = if (length fstr) > 0 then filter (isInfixOf (fstr::String)) namelist else namelist
                                         index' = case index of
                                           Nothing -> Nothing
                                           Just i -> (if (length rlist) > 0 then Just (rlist !! 0) else Nothing) where
                                             rlist = findIndices (== (namelist !! i)) namelist'
        returnA -< (namelist', index')
                
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
        
  
  let getSurnameW = mkFix (\t (names, index) -> case index of 
                           Just i -> Right $ takeWhile (/= ',') (names !! i)
                           Nothing -> Right "")
                
  let getNameW = mkFix (\t (names, index) -> case index of 
                           Just i -> Right ( drop 2 $ dropWhile (/= ',') (names !! i))
                           Nothing -> Right "")
                
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
                
  let fw = proc _ -> do
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
      
  return fw
  
  
getAddWindow m = do
  (GtG leb) <- getM m "leb"
  (GtG reb) <- getM m "reb"
  (GtG sum) <- getM m "sum"
  
  let (lebc, lebs) = editBoxW leb
  let (rebc, rebs) = editBoxW reb
  let sums = staticTextW sum
      
  let w1 = sums . (plusF <$> hold "0.1" lebc <*> hold "0.1" rebc)
  let w2 = lebs . (once . pure "0.0")
  let w3 = rebs . (once . pure "0.0")
  let fw = empty . w1 <|> empty . w2 <|> empty . w3 <|> pure GtNone :: GameWire GameType GameType
  return fw

getCntWindow m = do
  (GtG buu) <- getM m "buu"
  (GtG bud) <- getM m "bud"
  (GtG cnt) <- getM m "cnt"
  
  let bu = buttonW buu
  let bd = buttonW bud
  let cnts = staticTextW cnt
      
  let w1 = (once . pure "0")
  let w2 = accum1 (\x -> (+x)) 0 . ( (bu . pure 1) <|> (bd . pure (-1)) )
  let w3 = cnts . (show <$> w2) :: GameWire a String
  let fw = empty . w1 <|> empty . w3 <|> pure GtNone :: GameWire GameType GameType
  return fw

floatW widget = (fchanged, fsetter) where
  sToF str = if length arr > 0 then fst $ arr !! 0 else 0.0 where
    arr = (reads str) :: [(Float, String)]
  toFW = mkFix (\t s -> Right $ sToF s)
  showF = mkFix (\t f -> Right $ show f)
  (changed, setter) = editBoxW widget
  fchanged = toFW . changed
  fsetter = toFW . setter . showF

getCurrWindow m = do
  (GtG eeb) <- getM m "eeb"
  (GtG deb) <- getM m "deb"
  
  let (eurc, eurs) = floatW eeb
  let (dolc, dols) = floatW deb
      
  let w1 = eurs . (once . pure 0.0)
  let w2 = dols . (once . pure 0.0)
  
  let w3 = proc _ -> do
        rec
          dollars <- delay 0.0 . (dolc <|> id) -< euros / 2.0
          euros <- eurc <|> id -< dollars * 2.0
                    
        dollars' <- dols . changed -< dollars
        euros' <- eurs . changed -< euros
        returnA -< dollars
  
  let fw = empty . w1 <|> empty . w2 <|> empty . w3 <|> pure GtNone :: GameWire GameType GameType
  return fw




runFunc wire map = do
      -- initialize the 3D environment
      hg <- init3D
      (dummy, hg2) <- run3D hg (initialize3DComponents map)
      (r2, hg3) <- run3D hg2 (renderLoop 40 wire renderStep) 
      return ()

fFromS :: String -> Float
fFromS s = if length a > 0 then fst (a !! 0) else 0.0 where
  a = reads s

plusF a b = show $ fFromS a + fFromS b :: String

printW = (mkFixM (\t s -> do
                     liftIO $ print s
                     return $ Right s)) :: GameWire String String


startWorld = do
  -- do a fitting wire
  (r, wire) <- mkRefWire (id :: GameWire GameType GameType) 
  map <- createM
  forkOS (runFunc (GtNone, wire) map)
  return (r, map)

main = do
  (r, wire) <- mkRefWire (id :: GameWire GameType GameType) 
  map <- createM
  hg <- init3D
  (dummy, hg2) <- run3D hg (initialize3DComponents map)
  fw1 <- getAddWindow map
  fw2 <- getCurrWindow map
  fw3 <- getCntWindow map
  fw4 <- getMenueWindow map
  fw5 <- getCrudWindow map
  let fw = empty . fw1 <|> empty . fw2 <|> empty . fw3 <|> empty . fw4 <|> empty . fw5 <|> pure GtNone 
  swapRefWire r fw
  (r2, hg3) <- run3D hg2 (renderLoop 40 (GtNone, wire) renderStep) 
  return ()
  
  





