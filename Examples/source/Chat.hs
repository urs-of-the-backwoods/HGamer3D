module Main where

import HGamer3D.Data
import HGamer3D.Common
import HGamer3D.Network

import Control.Monad
import Control.Concurrent
import System.Environment

clientWork name = do

  world <- forkNetworkWorld (msecT 50)

  let c1 = ClientNode 8088
  ec <- newE [CTNNo #: c1 ]
  addToWorld world ec

  regEvtH ec (\ml -> mapM (\m -> do
                    case m of
                         DataMessage name msg -> print (name ++ ": " ++ msg)
                         ClientConnectedToServer ip port -> print ("client connected to server: " ++ ip ++ ":" ++ (show port))
                         ClientConnectionProcessStarted ip port -> print ("client tries to connect to server: " ++ ip ++ ":" ++ (show port))
                         _ -> return ()
                         ) ml >> return ())
  
  let loop = do
           inline <- getLine
           sendCmd ec (SendToServer inline)
           loop

  loop

serverWork = do

  world <- forkNetworkWorld (msecT 50)

  let s1 = ServerNode 8088 8090
  es <- newE [CTNNo #: s1 ]
  addToWorld world es

  sleepFor (msecT 100) -- give time to create, before sending commands

  let bLoop = do
      sendCmd es BroadcastServerInformation
      sleepFor (secT 60)
      bLoop

  forkIO bLoop

  regEvtH es (\ml -> mapM (\m -> do
                    case m of
                         DataMessage name msg -> print (name ++ ": " ++ msg)
                         _ -> return ()
                           ) ml >> return ())

  let loop = do
      sleepFor (secT 1)
      loop
  loop

   
usagemsg = "usage: chat (client|server)"

main = do
     args <- getArgs
     if (length args) == 1 then
        if (args !! 0) == "server" then serverWork
        else if (args !! 0) == "client" then clientWork (args !! 1)
             else print usagemsg
     else print usagemsg

