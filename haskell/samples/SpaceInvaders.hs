{-# LANGUAGE OverloadedStrings #-}
module Main where
import HGamer3D

import Control.Concurrent
import Control.Monad
import qualified Data.Map as M
import qualified Data.Text as T
import Data.Maybe

-- pure data definitions
-- ---------------------

data ActorType = Canon | Boulder | Invader Int | Shot deriving (Eq, Show, Ord)

-- HGamer3D website, code space invaders 2d, artwork
arts :: M.Map ActorType (T.Text, T.Text, Int, Int)
arts = M.fromList [ 
  (Canon, ("___/=\\___\n###   ###", "", 76, 30)),
  (Boulder, ("  #####\n###   ###\n###   ###", "", 70, 45)), 
  (Invader 1, ("  X_X\n\"# # #\"\n\" < > \"", "\" X_X \"\n\"# # #\"\n  > <", 54, 45)),
  (Invader 2, ("   ___\n ++###++\n##__#__##\n X  =  X", "  ___\n ++###++\n++__#__++\n L  =  L", 70, 60)),
  (Invader 3, (" #+++#\n# . . #\nx-   x-", " #+++#\n# . . #\n-x   -x", 54, 45)),
  (Shot, ("\"\"", "", 14, 4))
  ]
-- end of website text  

instance Show Entity where
  show e = "Entity: "
  
data Actor = Actor ActorType Entity deriving (Eq, Show)

canonBouldersData :: [(ActorType, Int, Int)]
canonBouldersData = [
  -- canon
  (Canon, 100, 550),

  -- boulders
  (Boulder, 40, 500),
  (Boulder, 200, 500),
  (Boulder, 360, 500),
  (Boulder, 520, 500),
  (Boulder, 680, 500)
  ]
  
invaderData :: [(ActorType, Int, Int)]
invaderData = [
  -- invaders 1
  ((Invader 1), 10, 210),
  ((Invader 1), 90, 210),
  ((Invader 1), 170, 210),
  ((Invader 1), 250, 210),
  ((Invader 1), 330, 210),
  ((Invader 1), 410, 210),
  ((Invader 1), 490, 210),
  ((Invader 1), 570, 210),
  
  ((Invader 1), 10, 150),
  ((Invader 1), 90, 150),
  ((Invader 1), 170, 150),
  ((Invader 1), 250, 150),
  ((Invader 1), 330, 150),
  ((Invader 1), 410, 150),
  ((Invader 1), 490, 150),
  ((Invader 1), 570, 150),
  
  -- invaders 2
  
  ((Invader 3), 10, 10),
  ((Invader 3), 115, 10),
  ((Invader 3), 220, 10),
  ((Invader 3), 325, 10),
  ((Invader 3), 430, 10),
  ((Invader 3), 535, 10),
  
  ((Invader 3), 10, 75),
  ((Invader 3), 115, 75),
  ((Invader 3), 220, 75),
  ((Invader 3), 325, 75),
  ((Invader 3), 430, 75),
  ((Invader 3), 535, 75)
  ]

-- actors and collisions
------------------------

-- HGamer3D website, code space invaders 2d, create actors
createActor :: HG3D -> ActorType -> Int -> Int -> IO Actor
createActor hg3d atype x y = do
  let (t, _, w, h) = fromJust (M.lookup atype arts)
  e <- newE hg3d [ ctText #: t, ctScreenRect #: Rectangle x y w h]
  return $ Actor atype e
                
createActors hg3d actorData = mapM (\(a, x, y) -> createActor hg3d a x y) actorData
-- end of website text  

posActor (Actor _ e) = readC e ctScreenRect >>= \(Rectangle x y _ _) -> return (x, y)
moveActor (Actor _ e) (x, y) = updateC e ctScreenRect (\(Rectangle a b c d) -> (Rectangle (a+x) (b+y) c d))
-- HGamer3D website, actions and do notation, first example
swapPic (Actor atype e) = do
  let (t1, t2, w, h) = fromJust (M.lookup atype arts)
  oldt <- readC e ctText
  if oldt == t1 
    then setC e ctText t2 >> return ()
    else setC e ctText t1 >> return ()
-- end of website text  

hit (Rectangle x y w h) (Rectangle x' y' w' h') = (not (x > x' + w' || x' > x + w)) && (not (y > y' + h' || y' > y + h)) 

getCollisions :: Actor -> [Actor] -> IO [Actor]
getCollisions (Actor a e) actors = 
  mapM (\(a'@(Actor _ e')) -> 
    if e /= e' 
      then
        hit <$> readC e ctScreenRect <*> readC e' ctScreenRect >>= \bHit ->
        return (if bHit then [a'] else [])
      else
        return []
  ) actors >>= return . concat


-- music and sound
------------------
  
music hg3d = newE hg3d [ 
  ctSoundSource #: Music "Sounds/RMN-Music-Pack/OGG/CD 3 - Clash of Wills/3-04 Joyful Ocean.ogg" 1.0 True "Music", 
  ctPlayCmd #: Stop ] >>=
  \m -> setC m ctPlayCmd Play

sounds hg3d = do
  ping <- newE hg3d [ ctSoundSource #: Sound "Sounds/inventory_sound_effects/ring_inventory.wav" 1.0 False "Sounds"
               , ctPlayCmd #: Stop ] -- creates a sound
  clash <- newE hg3d [ ctSoundSource #: Sound "Sounds/inventory_sound_effects/metal-clash.wav" 1.0 False "Sounds"
              , ctPlayCmd #: Stop ] -- creates another sound
  return (ping, clash)


-- key handling
---------------

-- HGamer3D website, code space invaders 2d, move canon
data CanonMove = NotMoving | MovingRight | MovingLeft
     deriving (Show)

handleKey k (varMoveState, varNumShots) = 
  case k of
    (KeyDown _ _ "Right") -> writeVar varMoveState MovingRight >> return ()
    (KeyUp _ _ "Right") -> writeVar varMoveState NotMoving >> return ()
    (KeyDown _ _ "Left") -> writeVar varMoveState MovingLeft >> return ()
    (KeyUp _ _ "Left") -> writeVar varMoveState NotMoving >> return ()
    (KeyDown _ _ "Space") -> updateVar varNumShots (\n -> (n + 1, ()))
    _ -> return ()

installKeyHandler hg3d varMoveState varNumShots = do
  ieh <- newE hg3d [ctInputEventHandler #: DefaultEventHandler, ctKeyEvent #: NoKeyEvent]
  registerCallback hg3d ieh ctKeyEvent (\k -> handleKey k (varMoveState, varNumShots))
-- end of website text
  

  
-- canon movements, shooting
----------------------------

canonLoop hg3d canon varShots varMoveState varNumShots = do
  (x, y) <- posActor canon
  moving <- readVar varMoveState
  isShot <- updateVar varNumShots (\n -> if n > 0 then (n-1, True) else (0, False))
  case moving of
    MovingLeft -> if x > 5 then moveActor canon (-5, 0) else return ()
    MovingRight -> if x < 720 then moveActor canon (5, 0) else return ()
    _ -> return ()
  if isShot
    then createActor hg3d Shot (x + 28) (y - 6) >>= \s -> updateVar varShots (\l -> (s:l, ()))
    else return ()
  sleepFor (msecT 20)
  canonLoop hg3d canon varShots varMoveState varNumShots

  
-- bullets flying
-----------------

shotsLoop varShots = do
  shots <- readVar varShots
  mapM (\shot -> moveActor shot (0, -15) ) shots
  sleepFor (msecT 20)
  shotsLoop varShots
  
  
-- invaders stepping
-------------------

invadersLoop varInvaders goRight countPic varEnd = do
  invaders <- readVar varInvaders
  changeDir <- mapM (\invader -> do
    moveActor invader (if goRight then (5,0) else (-5, 0))
    pos <- posActor invader
    if countPic > 4 
      then (swapPic invader) 
      else return ()
    if fst pos > 700 
      then return (-1) 
      else if fst pos < 50 
            then return 1 
            else return 0
    ) invaders :: IO [Int]
  let newDirection = if 1 `elem` changeDir then True else if -1 `elem` changeDir then False else goRight
  if -1 `elem` changeDir 
    then mapM (\invader -> moveActor invader (0, 40)) invaders >> return ()
    else return ()
  sleepFor (msecT 100)
  end <- readVar varEnd
  if end == 0 
    then invadersLoop varInvaders newDirection ( if countPic > 4 then 0 else countPic + 1) varEnd
    else return ()
 
 
-- collision effects
--------------------

collisionLoop varInvaders varShots varEnd boulders ping clash = do
  shots <- readVar varShots
  invaders <- readVar varInvaders
  ps <- mapM (\shot -> 
    do
      bs <- getCollisions shot boulders
      is <- getCollisions shot invaders
      if length bs > 0 
        then return [(bs !! 0, shot)] 
        else if length is > 0 
          then setC ping ctPlayCmd Play >> return [(is !! 0, shot)]
          else return []
    ) shots >>= return . concat
  let killShots = map snd ps
  let killInv = filter (\(Actor atype _) -> 
                    case atype of 
                      Invader n -> True
                      _ -> False ) (map fst ps)
  updateVar varShots (\ss -> (filter (\s -> not (s `elem` killShots)) ss, ()))
  updateVar varInvaders (\is -> (filter (\i -> not (i `elem` killInv)) is, ()))
  numI <- readVar varInvaders >>= return . length
  if numI == 0 then updateVar varEnd (\_ -> (1, ())) else return ()
  mapM (\a -> moveActor a (1000, 1000) ) (killShots ++ killInv)

  mapM (\b -> do
    bis <- getCollisions b invaders
    if length bis > 0
      then setC clash ctPlayCmd Play >> updateVar varEnd (\_ -> (-1, ()))
      else return ()
    ) boulders

  sleepFor (msecT 40)
  collisionLoop varInvaders varShots varEnd boulders ping clash
 

handleEnd hg3d varEnd = do
  end <- readVar varEnd
  if end > 0 
    then  newE hg3d [  ctText #: "Congratulations, you won!", 
                  ctScreenRect #: Rectangle 300 180 100 30] >> sleepFor (secT 10) >> exitHG3D hg3d
    else if end < 0
      then newE hg3d [ ctText #: "The invaders got you! Try again!", 
                  ctScreenRect #: Rectangle 300 180 100 30] >> sleepFor (secT 10) >> exitHG3D hg3d
      else return ()
  sleepFor (secT 1)
  handleEnd hg3d varEnd

-- HGamer3D website, code space invaders 2d, game logic
gameLogic hg3d = do    
  music hg3d
  (ping, clash) <- sounds hg3d
  
  (canon : boulders) <- createActors hg3d canonBouldersData
  invaders <- createActors hg3d invaderData
  
  varInvaders <- makeVar invaders
  varMoveState <- makeVar NotMoving
  varNumShots <- makeVar 0
  varShots <- makeVar []
  varEnd <- makeVar 0

  installKeyHandler hg3d varMoveState varNumShots
  
  forkIO (canonLoop hg3d canon varShots varMoveState varNumShots)
  forkIO (shotsLoop varShots)
  forkIO (invadersLoop varInvaders True 0 varEnd)
  forkIO (collisionLoop varInvaders varShots varEnd boulders ping clash)
  forkIO (handleEnd hg3d varEnd)

  return ()
-- end of website text  
  
main = do
  runGame standardGraphics3DConfig gameLogic (msecT 20)
  return ()
