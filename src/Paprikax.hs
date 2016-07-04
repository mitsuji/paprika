module Paprikax (
  leftOn
  ,leftOff
  ,leftReverse
  ,rightOn
  ,rightOff
  ,rightReverse
  ,forward
  ,stop
  ,backward
  ,forwardLeft
  ,forwardRight
  ,backwardLeft
  ,backwardRight
  ,turnLeft
  ,turnRight
  ) where

import System.Huckleberry.Linux.Gpio (setValue)
import Control.Concurrent (threadDelay)


leftOn = setValue 78 True >> setValue 79 False
leftOff = setValue 78 False >> setValue 79 False
leftReverse = setValue 78 False >> setValue 79 True

rightOn = setValue 80 True >> setValue 81 False
rightOff = setValue 80 False >> setValue 81 False
rightReverse = setValue 80 False >> setValue 81 True


stop = leftOff >> rightOff

forward = leftOn >> rightOn
backward = leftReverse >> rightReverse

forwardLeft = leftOff >> rightOn
forwardRight = leftOn >> rightOff

backwardLeft = leftOff >> rightReverse
backwardRight = leftReverse >> rightOff

turnLeft = leftReverse >> rightOn
turnRight = leftOn >> rightReverse


forward' n = forward >> threadDelay (n * 1000) >> stop
backward' n = backward >> threadDelay (n * 1000) >> stop

forwardLeft' n = forwardLeft >> threadDelay (n * 1000) >> stop
forwardRight' n = forwardRight >> threadDelay (n * 1000) >> stop

backwardLeft' n = backwardLeft >> threadDelay (n * 1000) >> stop 
backwardRight' n = backwardRight >> threadDelay (n * 1000) >> stop

turnLeft' n = turnLeft >> threadDelay (n * 1000) >> stop
turnRight' n = turnRight >> threadDelay (n * 1000) >> stop


test1 = forward' 2000 >> forwardLeft' 1000 >> forward' 2000 >> forwardRight' 2000 >> forward' 2000 >> turnLeft' 3000
