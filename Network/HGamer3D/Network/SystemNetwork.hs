{-# Language StandaloneDeriving, DeriveDataTypeable #-}
{-# OPTIONS_HADDOCK hide #-}

-- This source file is part of HGamer3D
-- (A project to enable 3D game development in Haskell)
-- For the latest info, see http://www.hgamer3d.org
--
-- (c) 2015 Peter Althainz
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

module HGamer3D.Network.SystemNetwork

where

import Data.Maybe
import Data.Typeable
import Data.Dynamic
import Data.Traversable as T
import Data.IORef
import Control.Concurrent
import Control.Monad
import qualified Data.Map as M

import HGamer3D.Data
import HGamer3D.Common
import qualified HGamer3D.BaseAPI.Network as NB
import HGamer3D.Network.PeerLookup
import qualified HGamer3D.Network.Node as NN

{-
        The network system supports two types of nodes, clients and servers. Also a type of auto connect is supported.
        Clients are enet clients, they are able to connect to servers. Servers are enet servers which can handle multiple
        clients. Once interconnect both types of nodes are able to send and receive packages to known peers.
        
        Connection setup is handled differently, Clients listens for broadcast of servers, server are able to broadcast.
        Once a client has received a broadcast, it connects to the server. Starting then, both nodes can exchange messages.
        
        Clients:
                - listening for incoming broadcast messages from servers
                - upon connection, they send a first "ping" message to announce their existence and name
                  (normally not needed in enet, but due to insufficient binding)
        
        Servers:
                - understand BroadcastServer command to broadcast their own existence
                - filtering incoming messages for ping messages to populate list of clients

        Multithreading:
                - in the main loop the system listens on all connections and transforming incoming messages into events
                - in a parallel loop the client node listens for broadcast messages

-}


-- Client functions and data

data ClientData = ClientData {
     cdClient :: NB.NetworkClient,
     cdConnected :: IORef Bool,
     listenId :: ThreadId
}

firstSuccess :: (a -> IO Bool) -> [a] -> IO (Maybe a)
firstSuccess f inList = foldl (\a b -> a >>= \r -> case r of 
                                                 Nothing -> f b >>= \r' -> return (if r' then Just b else Nothing)
                                                 Just v -> return (Just v)) (return Nothing) inList

getNewPeers :: NB.NetworkClient -> ERef -> Int -> IORef Bool -> IO ()
getNewPeers client eref bcPort rCon = do
                    receiveInfoServer (show bcPort) handler where
                         handler str = do
                               let (port, addrList) = decodePortIPs str
                               connected <- readIORef rCon
                               if not connected then do
                                     mAddr <- firstSuccess (\addr -> sendEvt eref [NN.ClientConnectionProcessStarted addr port] >> NB.connectClientToServer client addr port) addrList
                                     case mAddr of
                                          Just con -> writeIORef rCon True >> sendEvt eref [NN.ClientConnectedToServer con port]
                                          Nothing -> return ()
                                     else return ()

createClient :: NN.NetworkNode -> ERef -> IO ClientData
createClient (NN.ClientNode bcPort) eref = do
     client <- NB.networkClient
     con <- newIORef False
     lid <- forkIO $ getNewPeers client eref bcPort con
     return $ ClientData client con lid

-- Server functions and data

data ServerData = ServerData {
     sdServer :: NB.NetworkServer,
     sdClients :: IORef [String],
     sdPort :: Int,
     sdBCPort :: Int
}

createServer :: NN.NetworkNode -> IO ServerData
createServer (NN.ServerNode bcPort port) = do
     server <- NB.networkServer port
     clients <- newIORef []
     return $ ServerData server clients port bcPort

-- Nodes, the real data of one node

data NetworkNode = ClientNode ClientData
                 | ServerNode ServerData

-- create network node from pure data definition out of Schema

networkNode :: NN.NetworkNode -> ERef -> IO NetworkNode
networkNode node eref = case node of
                        (NN.ServerNode _ _) -> createServer node >>= (\d -> return (ServerNode d))
                        (NN.ClientNode _) -> createClient node eref >>= (\d -> return (ClientNode d))

updateNode :: NetworkNode -> ERef -> NN.NetworkNode -> IO NetworkNode
updateNode oldNode eref newNode = do
           removeNode oldNode
           node <- networkNode newNode eref
           return node

removeNode :: NetworkNode -> IO ()
removeNode node = return ()


-- doCmd, handle commands, available for network nodes

doCmd :: NetworkNode -> Maybe NN.NetworkCommand -> IO ()
doCmd node cmd = do
      case (node, cmd) of
           (ClientNode client, Just (NN.SendToServer msg)) -> NB.sendNetworkMessage (cdClient client) "not used" 1 msg 
           (ServerNode server, Just NN.BroadcastServerInformation) -> do
                                                                ips <- getOwnIpAddresses
                                                                let msg = encodePortIPs (sdPort server) ips
                                                                broadcastInfo (show (sdBCPort server)) msg
                                                                return ()
           (ServerNode server, Just (NN.SendToClient name msg)) -> NB.sendNetworkMessage (sdServer server) name 1 msg
           _ -> return ()


-- the network system data
                                 
data ECSNetwork = ECSNetwork {
     network :: NB.NetworkSystem,
     nodes :: IORef [(IORef NetworkNode, ERef)]
     }

     
instance System ECSNetwork where

    initializeSystem = do
         
      lock <- newMVar ()
      newERefs <- newIORef []
      delERefs <- newIORef []
      let records = []
      network <- NB.initNetwork
      nodelist <- newIORef []
      let system = ECSNetwork network nodelist

      let systemFunction system eref = do

          e <- readE eref -- this e is used to create the representation

          if e #? CTNNo then do

             rep <- networkNode ((e # CTNNo) :: NN.NetworkNode) eref
             ref <- newIORef rep
             ns <- readIORef (nodes system)
             writeIORef (nodes system) ((ref, eref) : ns)

             let createRecord ct = do
                 if e #? ct then do
                    l <- componentListener eref ct
                    let uf = case ct of
                                  CTNNo -> \ _ e' -> do
                                                              rep <- readIORef ref
                                                              rep' <- updateNode rep eref (e' # CTNNo)
                                                              writeIORef ref rep'
                                                              return () 
                                  CTCmd -> \ _ e' -> do
                                                              rep <- readIORef ref
                                                              doCmd rep (e' ?# CTCmd)
                                                              return ()
                                  _ -> \ _ _ -> return ()

                    let df = return ()
                    return $ Just (l, uf, df)
                    else return Nothing

             newRecords <- Prelude.mapM createRecord [CTNNo, CTCmd]
             return (map fromJust (filter isJust newRecords))

             else return []
        
      return (SystemData lock newERefs delERefs records system systemFunction)

    stepSystem (SystemData lock newERefs delERefs records system systemFunction) = do
      nodelist <- readIORef (nodes system)
      Control.Monad.mapM ( \(rnode, eref) -> do
                                        -- listen for data
                                        n <- readIORef rnode
                                        pkgs <- case n of 
                                                            ClientNode node -> do
                                                                       isCon <- readIORef (cdConnected node)
                                                                       if isCon then (NB.receiveNetworkMessages (cdClient node) (msecT 0)) else return []
                                                            ServerNode node -> NB.receiveNetworkMessages (sdServer node) (msecT 0)
                                        let msgs = map (\pkg -> NN.DataMessage (NB.clientname pkg) (NB.message pkg)) pkgs
                                        when (length msgs > 0) (sendEvt eref msgs)

           ) nodelist
      return False

forkNetworkWorld :: GameTime -> IO [SomeSystem]
forkNetworkWorld sleepT = do
                       system <- (runSystem sleepT) :: IO (SystemData ECSNetwork)
                       return $ [SomeSystem system]


{-

startTest = do
        system <- forkNetworkWorld (msecT 10)
        
        let c1 = NN.NetworkNode "client1" NN.ClientNode 8088 8090
        let s1 = NN.NetworkNode "server1" NN.ServerNode 8088 8090
        ec <- newE [CTNNo #: c1 ]
        es <- newE [CTNNo #: s1 ]

        Control.Monad.mapM (addToWorld system) [ec, es]
        return (ec, es)

        (ec, es) <- startTest
        sendCmd es NN.BroadcastServer
        sendCmd ec (NN.SendToServer "Hi you")

        regEvtH es ( (\ml -> print (length ml)) :: [NN.NetworkMessage] -> IO ())
-}


