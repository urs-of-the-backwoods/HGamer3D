{-# OPTIONS_HADDOCK hide #-}
-- This source file is part of HGamer3D
-- (A project to enable 3D game development in Haskell)
-- For the latest info, see http://www.hgamer3d.org
--
-- (c) 2011-2013 Peter Althainz
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

-- Network/Internal/Base.hs

-- | Network functionality for HGamer3D, internal implementation, which exports also internal data structures. Public API is in module HGamer3D.Network.
module HGamer3D.Network.Internal.Base
where

import GHC.Ptr

import HGamer3D.Data.HG3DClass
import HGamer3D.Data.TimeMS

import HGamer3D.Bindings.Enet.ClassEnet as EnetClass
import HGamer3D.Bindings.Enet.ClassEnetServer as EnetServer
import HGamer3D.Bindings.Enet.ClassEnetClient as EnetClient
import HGamer3D.Bindings.Enet.ClassEnetPacket as EnetPacket

-- | The network system
data NetworkSystem = NetworkSystem HG3DClass

-- | The network client
data NetworkClient = NetworkClient HG3DClass

-- | The network server
data NetworkServer = NetworkServer HG3DClass

-- |  A network packet
data NetworkPacket = NetworkPacket {
                clientname :: String,  -- ^ the name of the client, sending the message, can be used, to reply
                channel :: Int, -- ^ the channel on which the message has been sent
                message :: String -- ^ the actual message itself
                }


initNetwork = do
	    hg3dclass <- EnetClass.new
	    return (NetworkSystem hg3dclass)

freeNetwork (NetworkSystem hg3dclass) = EnetClass.delete hg3dclass

-- | creates a network client, a client can connect to servers, but not vice versa
networkClient :: IO NetworkClient
networkClient = do
    client <- EnetClass.createClient
    return (NetworkClient client)
    
-- | creates a network server, a server can accept connections from clients
networkServer :: Int  -- ^ the port on which the server listen for connections
                       -> IO NetworkServer
networkServer port = do
    server <- EnetClass.createServer port
    return (NetworkServer server)

-- | create a connection from a client to a server
connectClientToServer :: NetworkClient -- ^ the client, created by createNetworkClient
                         -> String  -- ^ the server address (regular IP addresses or NS names)
                         -> Int -- ^ the port of the server to connect to
                         -> IO Bool
connectClientToServer (NetworkClient client) serveraddress port = do
    ok <- EnetClient.connect client serveraddress port
    return ok
    
-- | disconnect a client from a server
disconnectClient :: NetworkClient -- ^ the client
                    -> IO Bool
disconnectClient (NetworkClient client) = do
    ok <- EnetClient.disconnect client
    return ok

-- | The network node. This TypeClass contain the methods for sending and receiving data.
class NetworkNode a where
    -- | the method to send a message
    sendNetworkMessage :: a  -- ^ the node itself
                              -> String -- ^ the clientname (only used, when a server send to one of his clients). Can be obtained from the first message send by the client, see above in the 'NetworkPacket' type.
                              -> Int -- ^ the channel on which to send the message
                              -> String -- ^ the message
                              -> IO ()
        
    -- | receive pending messages
    receiveNetworkMessages :: a -- ^ the node itself
                                  -> TimeMS -- ^ the time in ms, which the node waits for incoming messages. If zero only delivers pending messages.
                                  -> IO [NetworkPacket] -- ^ array of received network packages


_transformPackage :: HG3DClass -> IO NetworkPacket
_transformPackage p = do
    message <- EnetPacket.getData p
    channel <- EnetPacket.getChannel p
    clientname <- EnetPacket.getPeer p
    EnetPacket.delete p
    return $ NetworkPacket clientname channel message

_receiveClient client ms pkg = do
    EnetClient.serve client ms
    p <- EnetClient.getPacket client
    let (HG3DClass aptr bptr) = p
    if aptr /= nullPtr 
        then do
            p' <- _transformPackage p
            pkg' <- _receiveClient client ms (pkg ++ [p'])
            return pkg'
        else do
            return pkg 
    
_receiveServer server ms pkg = do
    EnetServer.serve server ms
    p <- EnetServer.getPacket server
    let (HG3DClass aptr bptr) = p
    if aptr /= nullPtr 
        then do
            p' <- _transformPackage p
            pkg' <- _receiveServer server ms (pkg ++ [p'])
            return pkg'
        else do
            return pkg 
    
instance NetworkNode NetworkClient where
    
    sendNetworkMessage (NetworkClient client) clientname channel message = do
        EnetClient.send client message channel
        return ()

    receiveNetworkMessages (NetworkClient client) (TimeMS ms) = do
        allPs <- _receiveClient client ms []
        return allPs

instance NetworkNode NetworkServer where

    sendNetworkMessage (NetworkServer server) clientname channel message = do
        EnetServer.send server clientname message channel
        return ()

    receiveNetworkMessages (NetworkServer server) (TimeMS ms) = do
        allPs <- _receiveServer server ms []
        return allPs


    

{-$Overview

The commuication model of this library is as follows. There are clients and servers. Clients can connect to servers but not the other way around. So each server can have multiple clients. Both are nodes. Nodes can send and receive messages. 

Upon connection the port is defined on which servers and clients communicate with each other.

In addition to the port, channels exists, to enable parallel streams of messages on different channels.

A client has a name, to identify him. This name is communicated in the first packet from the client to the server. The server needs the client name, to send a package to the specific client. The client does not need a name for the server, since there exists only one connection from the client to one server.

-}
