-- This source file is part of HGamer3D
-- (A project to enable 3D game development in Haskell)
-- For the latest info, see http://www.hgamer3d.org
--
-- (c) 2011 - 2015 Peter Althainz
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

-- HGamer3D/Data/GameTime.hs

{-# LANGUAGE TypeSynonymInstances, CPP #-}

-- | The time type for HGamer3D
module HGamer3D.Data.GameTime

(
  GameTime,
  sec,
  msec,
  usec,
  
  secT,
  msecT,
  usecT,
  
  getTime,
  getThreadCPUTime,
  getProcessCPUTime,

  sleepFor
)

where

import Control.Concurrent
import qualified System.Clock as C

type GameTime = C.TimeSpec

_toInteger :: C.TimeSpec -> Integer
_toInteger ts = let
  s = (fromIntegral (C.sec ts)) :: Integer
  ns = (fromIntegral (C.nsec ts)) :: Integer
  in s * 1000000000 + ns

_fromInteger :: Integer -> C.TimeSpec
_fromInteger i = let
  s = fromIntegral (i `quot` 1000000000)
  ns = fromIntegral (i `rem` 1000000000)
  in C.TimeSpec s ns

-- | time to seconds
sec :: C.TimeSpec -> Int
sec ts = fromIntegral ((_toInteger ts) `quot` 1000000000)

-- | time to milliseconds
msec :: C.TimeSpec -> Int
msec ts = fromIntegral ((_toInteger ts) `quot` 1000000)

-- | time to microseconds
usec :: C.TimeSpec -> Int
usec ts = fromIntegral ((_toInteger ts) `quot` 1000)

-- | create time from seconds
secT :: Int -> C.TimeSpec
secT i = _fromInteger ((fromIntegral i) * 1000000000)

-- | create time from milliseconds
msecT :: Int -> C.TimeSpec
msecT i = _fromInteger ((fromIntegral i) * 1000000)

-- | create time from microseconds
usecT :: Int -> C.TimeSpec
usecT i = _fromInteger ((fromIntegral i) * 1000)

-- | get current time
getTime :: IO GameTime
getTime = C.getTime C.Realtime

-- | get thread time
getThreadCPUTime :: IO GameTime
getThreadCPUTime = C.getTime C.ThreadCPUTime

-- | get process cpu time
getProcessCPUTime :: IO GameTime
getProcessCPUTime = C.getTime C.ProcessCPUTime

-- | sleep for time intervall
sleepFor :: GameTime -> IO ()
sleepFor gt = threadDelay (usec gt)

