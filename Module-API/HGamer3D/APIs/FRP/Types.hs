{-# LANGUAGE Arrows #-}

module HGamer3D.APIs.FRP.Types

(
        mkRefWire,
        swapRefWire,
        MHGamer3D2,
        GameWire
)

where

import HGamer3D.APIs.Base as Base
import HGamer3D.Bindings.CEGUI.ClassHG3DEventController as HG3DEventController

import Control.Monad.Trans

import Control.Monad.Identity (Identity)
import Control.Wire
import Prelude hiding ((.), id)

import Control.Monad.Reader
import Control.Monad.State

import Control.Concurrent.MVar
import Data.IORef

type MHGamer3D2 = (ReaderT HG3DReaderState) (StateT HG3DEngineState IO)
type GameWire a b = Wire () MHGamer3D2 a b

-- function to keep a reference to a wire

mrefWireFunction :: Time -> (a, MVar (Wire e MHGamer3D2 a b)) -> MHGamer3D (Either e b, (MVar (Wire e MHGamer3D2 a b)))
mrefWireFunction dt (a, s) = do
        wire <- liftIO $ takeMVar s
        (r, wire') <- stepWire wire dt a
        liftIO $ putMVar s wire'
        return (r, s)

mkRefWire :: Wire e MHGamer3D2 a b -> IO (MVar (Wire e MHGamer3D2 a b), (Wire e MHGamer3D2 a b))
mkRefWire wire = do
           ref <- newMVar wire
           let wire' = mkStateM ref mrefWireFunction
           return (ref, wire')

swapRefWire :: MVar (Wire e MHGamer3D2 a b) -> Wire e MHGamer3D2 a b -> IO (Wire e MHGamer3D2 a b)
swapRefWire ref wire = swapMVar ref wire
