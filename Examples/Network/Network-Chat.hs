module Main where

import HGamer3D.Data.TimeMS
import HGamer3D.Network

--import Control.Monad.Trans

doClientWork client = do
	l <- getLine
	sendNetworkMessage client "server" 1 l
	doClientWork client
	

serverLoop server clients = do
	pkgs <- receiveNetworkMessages server (TimeMS 100)
	mapM (print . message) pkgs
	serverLoop server clients

doServerWork = do
	server <- createNetworkServer 7890
	serverLoop server []

-- connect to server on port 7890
action = do
        n <- initNetwork
	client <- createNetworkClient
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

