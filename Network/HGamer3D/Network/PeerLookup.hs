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

-- simple UDP broadcast to support lookup of peers in the same network
module HGamer3D.Network.PeerLookup
where

import Network.Socket
import Control.Exception
import Control.Concurrent
import Network.Info

import Numeric
import Data.List
import Data.List.Split

-- from http://blog.coldflake.com/posts/Simple-Networking/

-- send info to broadcast per UDP
broadcastInfo :: [Char] -> String -> IO ()
broadcastInfo port info = withSocketsDo $ bracket getSocket sClose talk
        where getSocket = do
                (serveraddr:_) <- getAddrInfo Nothing (Just "255.255.255.255") (Just port)
                s <- socket (addrFamily serveraddr) Datagram defaultProtocol
                connect s (addrAddress serveraddr) >> return s
              talk s = do
                send s info >> return ()

getOwnIpAddresses :: IO [String]
getOwnIpAddresses = do
                ns <- getNetworkInterfaces
                return $ map (show . ipv4) ns

receiveInfoServer :: [Char] -> (String -> IO()) -> IO ()
receiveInfoServer port shandler = withSocketsDo $ bracket connectMe sClose (handler shandler)
          where
            connectMe = do
              (serveraddr:_) <- getAddrInfo
                                  (Just (defaultHints {addrFlags = [AI_PASSIVE]}))
                                  Nothing (Just port)
              sock <- socket (addrFamily serveraddr) Datagram defaultProtocol
              bindSocket sock (addrAddress serveraddr) >> return sock

            handler shandler conn = do
                    (msg,n,d) <- recvFrom conn 1024
                    shandler msg
                    handler shandler conn


hex = \a -> showHex a ""
unhex = fst . head . readHex

encodeIP :: String -> String
encodeIP = concat . encodeHex . toIntArr . toStrArr where
                       toStrArr s = splitOn "." s
                       toIntArr s = map read s :: [Int]
                       encodeHex = map (\i -> let h = hex i in if length h < 2 then '0' : h else h)

encodeIPs :: [String] -> String
encodeIPs ips = concat $ map encodeIP ips

decodeIP :: String -> String
decodeIP ip =  intercalate "." (map (show . unhex ) (splitEvery 2 ip))

decodeIPs ips = map decodeIP (splitEvery 8 ips)

encodePortIPs :: Int -> [String] -> String
encodePortIPs port ips = (show port) ++ "." ++ (encodeIPs ips)

decodePortIPs :: String -> (Int, [String])
decodePortIPs code = (port, ips) where
                   l = splitOn "." code
                   port = read (head l)
                   ips = decodeIPs (head (tail l))
