{-# LANGUAGE Arrows #-}

-- | 3D graphics functionality of the FRP API
module HGamer3D.APIs.FRP.Graphics3D

(
        scaleW,
        translateW,
        scaleToW,
        directionW,
        directionToW,
        positionW,
        positionToW,
        orientationW,
        orientationToW
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

import HGamer3D.APIs.FRP.Types
import Data.IORef

-- | A wire, which delivers each cycle the position, input is ignored
positionW :: Position3D ob => ob -- ^ positionable object
             -> GameWire a Vec3 -- ^ wire which delivers the position
positionW ob = mkFixM (\t x -> do
                  vec <- position3D ob
                  return (Right vec))
                  
-- | A wire, which sets and returns the position
positionToW :: Position3D ob => ob -- ^ positionable object
             -> GameWire Vec3 Vec3 -- ^ wire which sets and returns the position
positionToW ob = mkFixM (\t vec -> do
                  positionTo3D ob vec
                  return (Right vec))
                  
-- | A wire, which translates each cycle the position               
translateW :: Position3D ob => ob -- ^ positionable object
              -> GameWire Vec3 Vec3 -- ^ wire, which translates the object
translateW ob = mkFixM (\t v -> do
                    translate3D ob v
                    return (Right v))
         
-- | A wire, which delivers each cycle the direction, input is ignored
directionW :: Direction3D ob => ob -- ^ object with direction
              -> GameWire a Vec3 -- ^ wire, which delivers the direction
directionW ob = mkFixM (\t x -> do
                  vec <- direction3D ob
                  return (Right vec))
                  
-- | A wire, which sets the direction                
directionToW :: Direction3D ob => ob -- ^ directionable object
                -> GameWire Vec3 Vec3 -- ^ wire, which sets direction
directionToW ob = mkFixM (\t v -> do
                    directionTo3D ob v
                    return (Right v))
         
-- | A wire, which delivers each cycle the scale, input is ignored
scaleW :: Scale3D ob => ob -- ^ scalable object
          -> GameWire a Vec3 -- ^ wire, which delivers the scale
scaleW ob = mkFixM (\t x -> do
                  vec <- scale3D ob
                  return (Right vec))
                  
-- | A wire, which sets the scale
scaleToW :: Scale3D ob => ob -- ^ scalable object
            -> GameWire Vec3 Vec3 -- ^ a wire, which sets and returns the scale
scaleToW ob = mkFixM (\t v -> do
                    scaleTo3D ob v
                    return (Right v))
         
-- | A wire, which delivers each cycle the orientation, input is ignored
orientationW :: Orientation3D ob => ob -- ^ object with orientation
                -> GameWire a UnitQuaternion -- ^ wire, which delivers the orientation, as a unit quaternion
orientationW ob = mkFixM (\t x -> do
                  ori <- orientation3D ob
                  return (Right ori))
                  
-- | A wire, which sets the orientation
orientationToW :: Orientation3D ob => ob -- ^ object with orientation
                  -> GameWire UnitQuaternion UnitQuaternion -- ^ a wire which sets and returns the orientation
orientationToW ob = mkFixM (\t ori -> do
                    orientationTo3D ob ori
                    return (Right ori))
         