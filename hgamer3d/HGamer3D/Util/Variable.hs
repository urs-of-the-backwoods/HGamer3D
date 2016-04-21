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

-- Variable.hs

-- | :-) I'm fundamentally convinced this is missing in Haskell, urs of the backwoods

module HGamer3D.Util.Variable
(
  Var,
  makeVar,
  readVar,
  writeVar,
  writeVar',
  updateVar,
  updateVar'
) where

import Data.IORef

newtype Var a = Var (IORef a)

makeVar :: a -> IO (Var a)
makeVar v = Var <$> newIORef v

readVar :: Var a -> IO a
readVar (Var r) = readIORef r

writeVar :: Var a -> a -> IO a
writeVar (Var r) val = atomicModifyIORef r (\old -> (val, old)) 

writeVar' :: Var a -> a -> IO a
writeVar' (Var r) val = atomicModifyIORef' r (\old -> (val, old))

updateVar :: Var a -> (a -> (a, b)) -> IO b
updateVar (Var r) f = atomicModifyIORef r f

updateVar' :: Var a -> (a -> (a, b)) -> IO b
updateVar' (Var r) f = atomicModifyIORef' r f

