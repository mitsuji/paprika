module Main where

import System.IO (hSetEcho,hSetBuffering,BufferMode(NoBuffering),stdin,stdout) 
import Paprikax

main :: IO ()
main = do
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering
  hSetEcho stdin False
  loop
  where
    loop = do
      ch <- getChar
      case ch of
        's' -> stop >> loop
        'f' -> forward >> loop
        'b' -> backward >> loop
        'l' -> forwardLeft >> loop
        'r' -> forwardRight >> loop
        't' -> turnRight >> loop
        'q' -> return ()
        _   -> loop


{-
backwardLeft = leftOff >> rightReverse
backwardRight = leftReverse >> rightOff

turnLeft = leftReverse >> rightOn
turnRight = leftOn >> rightReverse
-}

