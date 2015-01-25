{-# LANGUAGE DeriveDataTypeable, StandaloneDeriving #-}
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

-- Interface definitions for network nodes
module HGamer3D.Network.Node
where

import Data.Typeable

data NetworkNode = ServerNode Int Int   -- server node with broadcast port and connection port
                 | ClientNode Int       -- client node, only with broadcast port
                 deriving (Eq, Show, Typeable)

data NetworkMessage = DataMessage String String                 -- ^ data message received: peer sending, data
                    | ClientConnectedToServer String Int        -- ^ client successfully connected to server
                    | ClientConnectionProcessStarted String Int -- ^ client tries to connect to ip address
                    deriving (Eq, Show, Typeable)

data NetworkCommand = SendToServer String               -- ^ String is the message itself, this cmd is handled by clients
                    | SendToClient String String        -- ^ Identifier of client, message, this cmd is handled by servers
                    | BroadcastServerInformation        -- ^ broadcast server ip addresses, inviting clients to connect (server cmd)
                    deriving (Eq, Show, Typeable) 
