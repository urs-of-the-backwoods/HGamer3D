module Main  where

import HGamer3D

import Control.Concurrent
import Control.Monad
import System.Environment
import Data.IORef

-- book: env
-- create basic 3D environment, camera and lights
create3DEnvironment = do
  let camera = Camera (Frustum 5.0 5000.0 (Deg 40)) (Viewport 0 (Rectangle 0.0 0.0 1.0 1.0) black)
  let light = Light white white PointLight 
  let sky = SkyBox (ResourceMaterial "SkyBox/BlueSpace") 10.0
  sceneE <- newE [CTCam #: camera, CTScP #: SceneParameter white NoShadows sky]
  lightE1 <- newE [CTLig #: light, CTPos #: Vec3 0.0 90.0 (-200.0)]
  lightE2 <- newE [CTLig #: light, CTPos #: Vec3 0.0 (-90.0) (-300.0)]
  return (sceneE, lightE1, lightE2)
-- book

-- book: walls
-- create the walls
wallfig scale nx ny nz material =
     let
        f = 100.0
        fig = SimpleFigure Cube (ResourceMaterial material)
        tile x nx y ny z nz s@(Vec3 sx sy sz) fig = ( (Vec3 (sx*(x-(nx-1)/2)*f/nx) (sy*(y-(ny-1)/2)*f/ny) (sz*(z-(nz-1)/2)*f/nz)) , unitU, (Vec3 (sx/nx) (sy/ny) (sz/nz)), fig)
        tiles = map (\(x, y, z) -> tile x nx y ny z nz scale fig) [(x, y, z) | x <- [0..(nx-1)], y <- [0..(ny-1)], z <- [0..(nz-1)]]
     in CombinedFigure tiles

wall pos scale nx ny nz material = [CTFig #: wallfig scale nx ny nz material, CTPos #: pos]

walls = do
  bottomE <- newE $ wall (Vec3 0.0 (-250.0) (-800.0)) (Vec3 5.0 0.05 5.0) 3 1 3 "Ground/Grass1"
  topE <- newE $ wall (Vec3 0.0 (250.0) (-800.0)) (Vec3 5.0 0.05 5.0) 2 1 2 "Floor/Wood1"
  leftE <- newE $ wall (Vec3 (-250.0) 0.0 (-800.0)) (Vec3 0.05 5.0 5.0) 1 2 1 "Wall/Brick1"
  rightE <- newE $ wall (Vec3 (250.0) 0.0 (-800.0)) (Vec3 0.05 5.0 5.0) 1 2 1 "Wall/Brick1"
  return (bottomE, topE, leftE, rightE)
-- book

-- book: paddles
-- create the paddles
paddles world = do
  myPaddle <- newE $ wall (Vec3 30.0 (-30.0) (-400)) (Vec3 0.5 0.5 0.05) 1 1 1 "Floor/Steel1"
  otherPaddle <- newE $ wall (Vec3 0.0 100.0 (-1200)) (Vec3 0.5 0.5 0.05) 1 1 1 "Floor/Steel2"
  mapM (addToWorld world) [myPaddle, otherPaddle]
  return (myPaddle, otherPaddle)
-- book

-- book: joystick
-- code to get joystick, automoatically takes lowest connected joystick as input device

joystick joyNum = do
  jE <- newE [ CTJoI #: (JoystickInfo (Joystick joyNum) [JoystickAxisX, JoystickAxisY] []), CTJoV #: (JoystickValue False [] [])]
  let jHandler = \msg -> case msg of
                    (JoysticksConnected js) -> do
                                        if length js > joyNum then do
                                           updateE jE CTJoI $ const $ JoystickInfo (js !! joyNum) [JoystickAxisX, JoystickAxisY] []
                                           print ("using joystick: " ++ (show (js !! joyNum)))
                                           else return ()
                    _ -> return ()
  regEvtH jE jHandler
  return jE

joyXY jE = do 
  v <- readE jE >>= \e -> return (e # CTJoV :: JoystickValue)
  case v of
       JoystickValue True [x, y] [] -> return (x, y)
       _ -> return (0.0, 0.0)


paddleFunction pos@(Vec3 px py pz) (x, y) = let
               speed = 0.5
               minimax mini maxi v = min maxi (max mini v)
               px' = minimax (-200.0) 200.0 (px + speed * x)
               py' = minimax (-150.0) 150.0 (py + speed * (-y))
               in (Vec3 px' py' pz)

paddleMover jE mP nodeE sFlag clientRef = forkIO $ forever $ do 
            xy <- joyXY jE
            updateE mP CTPos (\v -> paddleFunction v xy)
            if sFlag then do
               mbClient <- readIORef clientRef
               case mbClient of
                    Just client -> do
                         v <- readE mP
                         let (Vec3 x y z) = v # CTPos
                         sendCmd nodeE (SendToClient client (show (ServerData zeroVec3 (x, y))))
                    _ -> return ()
               return ()
--               sendCmd nodeE (SendToClient 
               else do
                    v <- readE mP
                    let (Vec3 x y z) = v # CTPos
                    sendCmd nodeE (SendToServer (show (ClientData (x, y))))
            sleepFor (msecT 50)

-- book

-- book: network

data SyncMessage = ServerData Vec3 (Float, Float)                   -- ball, paddle
                 | ClientData (Float, Float)                        -- paddle
                 deriving (Read, Show)

networkClient  bE psE pcE = do
  node <- newE [CTNNo #: ClientNode 8088 ]
  regEvtH node (\ml -> mapM (\m -> do
                    case m of
                         DataMessage name msg -> do
                                     let syncMsg = read msg
                                     case syncMsg of
                                          ServerData ballPos (pcX, pcY) -> do
--                                                     updateE bE CTPos $ const $ ballPos
                                                     updateE psE CTPos $ (\(Vec3 x y z) -> Vec3 pcX pcY z)
                                          _ -> return ()

                         ClientConnectedToServer ip port -> print ("client connected to server: " ++ ip ++ ":" ++ (show port))
                         ClientConnectionProcessStarted ip port -> print ("client tries to connect to server: " ++ ip ++ ":" ++ (show port))
                         _ -> return ()
                         ) ml >> return ())
  return node

networkServer pcE clientRef = do
  node <- newE [CTNNo #: ServerNode 8088 8090 ]
  forkIO $ forever $ do
      sleepFor (msecT 100)
      sendCmd node BroadcastServerInformation
      sleepFor (secT 10)
  regEvtH node (\ml -> mapM (\m -> do
                    case m of
                         DataMessage name msg -> do
                                     writeIORef clientRef (Just name)
                                     let syncMsg = read msg
                                     case syncMsg of
                                          ClientData (x, y) -> do
                                                     updateE pcE CTPos $ (\(Vec3 xo yo zo) -> Vec3 x y zo)
                                          _ -> return ()

                         _ -> return ()
                           ) ml >> return ())
  return node

run sFlag joyNum = do
  clientRef <- newIORef Nothing
  world <- forkHGamer3D
  (sceneE, lightE1, lightE2) <- create3DEnvironment
  (bE, topE, lE, rE) <- walls
  (mP, oP) <- paddles world
  jE <- joystick joyNum
  node <- if sFlag then networkServer oP clientRef else networkClient bE oP mP
  mapM (addToWorld world) [sceneE, lightE1, lightE2, bE, topE, lE, rE, mP, oP, jE, node]
  t1 <- paddleMover jE mP node sFlag clientRef
  return (world, sceneE, bE, topE, lE, rE, jE, mP, oP, t1)

usagemsg = "usage: paddle (client|server)"

main = do
  args <- getArgs
  let sFlag = if (length args) == 1 then
                 if (args !! 0) == "server" then True
                 else if (args !! 0) == "client" then False
                      else error usagemsg
              else error usagemsg

  (world, sceneE, bE, topE, lE, rE, jE, mP, oP, t1) <- run sFlag 0

  qvar <- regQuitHandler sceneE
  takeMVar qvar


{-

(world, sE, bE, topE, lE, rE, jE, mP, oP, t1) <- run True 1   -- server
(world, sE, bE, topE, lE, rE, jE, mP, oP, t1) <- run False 0  -- client
updateE mP CTPos $ const $ (Vec3 0 0 (-400) )
t2 <- paddleMover jE mP
killThread t2

-}

