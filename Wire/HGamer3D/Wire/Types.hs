{-# LANGUAGE Arrows, DeriveDataTypeable, StandaloneDeriving #-}

module HGamer3D.Wire.Types


where
  
import Data.IORef
import Data.Typeable  
import Data.Dynamic
import Data.List
import Data.Maybe
import qualified Data.Map as M

import HGamer3D.Graphics3D
import HGamer3D.Audio
import HGamer3D.InputSystem

import Control.Wire
import Control.Wire.Unsafe.Event
import Prelude hiding ((.), id)

-- One type for event map, unique names, guisystem, graphics3Dsystem
--------------------------------------------------------------------

data WireSystem = WireSystem {
     wsUniqueName :: UniqueName,
     wsG3d :: Graphics3DSystem,
     wsGui :: GUISystem,
     wsEvtDistMap :: (IORef (M.Map String (GUIEvent -> IO ())), IORef [(SDLEvent -> IO ())] ),
     wsGuiElList :: IORef [GUIElement]
    }

-- The basic Wire type
----------------------

type Time = Double
type RunState = (Timed NominalDiffTime ())
type GameWire = Wire RunState () IO

-- Entity Component System
--------------------------

-- this is a prototype implementation to show the potential
-- IORef's are solely used in the system data aka components of the engine
-- (reading some ECS literature, I thought implementing components as pure FRP might be a big hassle)
  
-- object ids will be simple string
type ObId = String

-- a component holds the "data", a state with an identity, there are different types of components
-- each component comes with its onwn type of system
data LocationC = LocationC (IORef Vec3) deriving (Typeable)
data OrientationC = OrientationC (IORef UnitQuaternion) deriving (Typeable)
data ObjectC = ObjectC Object3D deriving (Typeable)
data VelocityC = VelocityC (IORef Vec3) deriving (Typeable)

deriving instance Typeable UnitQuaternion
deriving instance Typeable Vec3

type Component = Dynamic
                   
-- an entity has an object id and components
data Entity = Entity { entId :: ObId, entComps :: [Component] } 

-- SYSTEMS for ECS

data SystemData d = SystemData {
  sdInitialize :: d,
  sdAddEntity :: Entity -> d -> d,
  sdRemoveEntity :: ObId -> d -> d,
  sdRunAction :: d -> RunState -> IO ()  -- only side effects on the IORef data, but complete set as input!
  }

-- all commands go into one type (for now)
data Command = CmdArr [Command]
               | CmdAddEntity Entity 
               | CmdRemEntity ObId
               | CmdMoveTo Vec3 Time
               | CmdNoOp
                 
          

-- administration function
--------------------------


registerGUIEventFunction :: WireSystem -> String -> (GUIEvent -> IO ()) -> IO ()
registerGUIEventFunction ws tag gevtF = do
  let (gedm, wedm) = wsEvtDistMap ws
  modifyIORef gedm (\m -> M.insert tag gevtF m)
  return ()

registerWinEventFunction :: WireSystem -> (SDLEvent -> IO ()) -> IO ()
registerWinEventFunction ws wevtF = do
  let (gedm, wedm) = wsEvtDistMap ws
  modifyIORef wedm (\l -> (wevtF : l))
  return ()

loadGuiLayout :: WireSystem -> String -> IO ()
loadGuiLayout ws name = do
  let guis = wsGui ws
  let refList = wsGuiElList ws
  mainwidget <- loadGuiLayoutFromFile guis name ""
  modifyIORef refList (\l -> (mainwidget : l))
  addGuiElToDisplay guis mainwidget
  return ()

getGuiWidget :: WireSystem -> String -> IO GUIElement
getGuiWidget ws name = do
  let ref = wsGuiElList ws
  lGE <- readIORef ref
  rlist <- mapM (\wg -> do
                    rval <- findChildGuiElRecursive wg name
                    return rval ) lGE
  let rval = case find isJust rlist of
        Just (Just val) -> val
        Nothing -> undefined      
  return rval   -- we assume, that element is available, otherwise runtime error
  
  
