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

import System.PIO.Linux.GPIO (setValue)
import Control.Concurrent (threadDelay)
import Prelude hiding ((!!),(||),(&&))


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


stop' n = stop >> threadDelay (n * 1000)

forward' n = forward >> threadDelay (n * 1000)
backward' n = backward >> threadDelay (n * 1000)

forwardLeft' n = forwardLeft >> threadDelay (n * 1000)
forwardRight' n = forwardRight >> threadDelay (n * 1000)

backwardLeft' n = backwardLeft >> threadDelay (n * 1000)
backwardRight' n = backwardRight >> threadDelay (n * 1000)

turnLeft' n = turnLeft >> threadDelay (n * 1000)
turnRight' n = turnRight >> threadDelay (n * 1000)



(!!) p n = p >> stop' n

(||) p n = p >> forward' n
(&&) p n = p >> backward' n

(!|) p n = p >> forwardLeft' n
(|!) p n = p >> forwardRight' n

(!&) p n = p >> backwardLeft' n
(&!) p n = p >> backwardRight' n

(&|) p n = p >> turnLeft' n
(|&) p n = p >> turnRight' n


test1 = forward' 2000 >> stop' 2000 >> forward' 2000 >> backward' 2000
        >> forwardLeft' 2000 >> forwardRight' 2000
        >> backwardLeft' 2000 >> backwardRight' 2000
        >> turnLeft' 2000 >> turnRight' 2000 >> stop
        
test2 = return () || 2000 !! 2000 || 2000 && 2000 !| 2000 |! 2000 !& 2000 &! 2000 &| 2000 |& 2000 !! 0



-- setValue 1 20000000 320000
-- setValue 1 20000000 1300000
-- setValue 1 20000000 2352000
