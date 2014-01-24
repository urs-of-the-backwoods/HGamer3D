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

-- Network.hs

-- | Network functionality of Base API
module HGamer3D.APIs.Base.Network.Network

(
  
                -- * Overview
                -- $Overview  
  
                -- * Types
		NetworkClient (..),
		NetworkServer (..),
		NetworkPacket (..),
		NetworkNode (..),
                
                -- * Connection Handling
		createNetworkClient,
		createNetworkServer,
		connectClientToServer,
		disconnectClient
)

where

import GHC.Ptr

import HGamer3D.Data.HG3DClass
import HGamer3D.Data.Vector

import Foreign.Ptr
import HGamer3D.Bindings.Enet.ClassEnet as Enet
import HGamer3D.Bindings.Enet.ClassEnetServer as EnetServer
import HGamer3D.Bindings.Enet.ClassEnetClient as EnetClient
import HGamer3D.Bindings.Enet.ClassEnetPacket as EnetPacket
import HGamer3D.Data.HG3DClass

import HGamer3D.APIs.Base.Engine.Types

import Control.Monad.Trans
import Control.Monad.Reader

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

-- | creates a network client, a client can connect to servers, but not vice versa
createNetworkClient :: MHGamer3D NetworkClient
createNetworkClient = do
	client <- liftIO $ Enet.createClient
	return (NetworkClient client)
	
-- | creates a network server, a server can accept connections from clients
createNetworkServer :: Int  -- ^ the port on which the server listen for connections
                       -> MHGamer3D NetworkServer
createNetworkServer port = do
	server <- liftIO $ Enet.createServer port
	return (NetworkServer server)

-- | create a connection from a client to a server
connectClientToServer :: NetworkClient -- ^ the client, created by createNetworkClient
                         -> String  -- ^ the server address (regular IP addresses or NS names)
                         -> Int -- ^ the port of the server to connect to
                         -> MHGamer3D Bool
connectClientToServer (NetworkClient client) serveraddress port = do
	ok <- liftIO $ EnetClient.connect client serveraddress port
	return ok
	
-- | disconnect a client from a server
disconnectClient :: NetworkClient -- ^ the client
                    -> MHGamer3D Bool
disconnectClient (NetworkClient client) = do
	ok <- liftIO $ EnetClient.disconnect client
	return ok

-- | The network node. This TypeClass contain the methods for sending and receiving data.
class NetworkNode a where
        -- | the method to send a message
	sendNetworkMessage :: a  -- ^ the node itself
                              -> String -- ^ the clientname (only used, when a server send to one of his clients). Can be obtained from the first message send by the client, see above in the 'NetworkPacket' type.
                              -> Int -- ^ the channel on which to send the message
                              -> String -- ^ the message
                              -> MHGamer3D ()
        
        -- | receive pending messages
	receiveNetworkMessages :: a -- ^ the node itself
                                  -> TimeMS -- ^ the time in ms, which the node waits for incoming messages. If zero only delivers pending messages.
                                  -> MHGamer3D [NetworkPacket] -- ^ array of received network packages


_transformPackage :: HG3DClass -> MHGamer3D NetworkPacket
_transformPackage p = do
	message <- liftIO $ EnetPacket.getData p
	channel <- liftIO $ EnetPacket.getChannel p
	clientname <- liftIO $ EnetPacket.getPeer p
	liftIO $ EnetPacket.delete p
	return $ NetworkPacket clientname channel message

_receiveClient client ms pkg = do
	liftIO $ EnetClient.serve client ms
	p <- liftIO $ EnetClient.getPacket client
	let (HG3DClass aptr bptr) = p
	if aptr /= nullPtr 
		then do
			p' <- _transformPackage p
			pkg' <- _receiveClient client ms (pkg ++ [p'])
			return pkg'
		else do
			return pkg 
	
_receiveServer server ms pkg = do
	liftIO $ EnetServer.serve server ms
	p <- liftIO $ EnetServer.getPacket server
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
		liftIO $ EnetClient.send client message channel
		return ()

	receiveNetworkMessages (NetworkClient client) (TimeMS ms) = do
		allPs <- _receiveClient client ms []
		return allPs

instance NetworkNode NetworkServer where

	sendNetworkMessage (NetworkServer server) clientname channel message = do
		liftIO $ EnetServer.send server clientname message channel
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