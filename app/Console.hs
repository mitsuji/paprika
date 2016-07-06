module Main where

import System.IO (hSetEcho,stdin) 
import Paprikax

main :: IO ()
main = hSetEcho stdin False >> loop
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

