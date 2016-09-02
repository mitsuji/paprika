module Main where

import System.IO (hSetEcho,hSetBuffering,BufferMode(NoBuffering),stdin,stdout) 
import System.Paprika

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
        'q' -> return ()
        _   -> do
          case ch of
            's' -> stop
            'f' -> forward
            'b' -> backward
            'l' -> forwardLeft
            'r' -> forwardRight
            't' -> turnRight
            _   -> return ()
          loop

