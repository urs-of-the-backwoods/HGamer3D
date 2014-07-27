module Main where

import HGamer3D.Data.GameTime
import HGamer3D.Network.BaseAPI

--import Control.Monad.Trans

doClientWork client = do
	l <- getLine
	sendNetworkMessage client "server" 1 l
	doClientWork client
	

serverLoop server clients = do
	pkgs <- receiveNetworkMessages server (msecT 100)
	mapM (print . message) pkgs
	serverLoop server clients

doServerWork = do
	server <- networkServer 7890
	serverLoop server []

-- connect to server on port 7890
action = do
        n <- initNetwork
	client <- networkClient
	ok <- connectClientToServer client "localhost" 7890
	if ok
		then do
			-- we are the client, proceed as client
			doClientWork client
			return ()
		else do
			-- we are the server, proceed as server
			doServerWork
			return ()
	
main = do
	action
	return ()

