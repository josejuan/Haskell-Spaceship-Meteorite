module TimeInterval (
  Time
, makeTime
, getElapsed
) where

{-- Windows
import System.Win32.Time (getTickCount)
import System.Win32.Types (DWORD)
import Control.Applicative

data Time = Time_ { previousTick :: DWORD }

makeTime :: IO Time
makeTime = Time_ <$> getTickCount

getElapsed :: Time -> IO (Double, Time)
getElapsed (Time_ t0) = do
    t@(Time_ t1) <- makeTime
    return (1e-3 * fromIntegral (t1 - t0), t)
--}

import System.Clock
import Control.Applicative

data Time = Time_ { previousTick :: TimeSpec }

makeTime :: IO Time
makeTime = Time_ <$> getTime Realtime

getElapsed :: Time -> IO (Double, Time)
getElapsed (Time_ t0) = do
    t@(Time_ t1) <- makeTime
    return (1e-9 * fromIntegral (timeSpecAsNanoSecs $ diffTimeSpec t0 t1), t)
