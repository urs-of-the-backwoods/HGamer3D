module Main where

import HGamer3D.APIs.Base
import Control.Monad.Trans

doClientWork client = do
	l <- liftIO $ getLine
	sendNetworkMessage client "server" 1 l
	doClientWork client
	

serverLoop server clients = do
	pkgs <- receiveNetworkMessages server (TimeMS 100)
	liftIO $ mapM (print . message) pkgs
	serverLoop server clients

doServerWork = do
	server <- createNetworkServer 7890
	serverLoop server []

-- connect to server on port 7890
action = do
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
	-- initialize HGamer3D
	hg <- initHGamer3D "HGamer3D - Network Chat Example" 
	-- run HGamer3D routines, here main 3D program
	(l, hg) <- runMHGamer3D hg action 
	return ()

