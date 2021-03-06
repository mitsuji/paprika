module System.Paprika (
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
  ,ArmLevel
  ,setLeftArm
  ,setRightArm
  ) where

import qualified System.PIO.Linux.GPIO as GPIO
import qualified System.PIO.Linux.PWM as PWM
import Control.Concurrent (threadDelay)
import Prelude hiding ((!!),(||),(&&))


{-

$ sh/setup_paprika.sh

[J18-pin1 ] <==> [1.8V <=> 3.3V] <==> [out3]
[J17-pin1 ] <==> [1.8V <=> 3.3V] <==> [out4]

[J20-pin11] <==> [out6]
[J20-pin12] <==> [out5]
[J20-pin13] <==> [out1]
[J20-pin14] <==> [out2]

-}


leftOn = GPIO.setValue 78 True >> GPIO.setValue 79 False
leftOff = GPIO.setValue 78 False >> GPIO.setValue 79 False
leftReverse = GPIO.setValue 78 False >> GPIO.setValue 79 True

rightOn = GPIO.setValue 80 True >> GPIO.setValue 81 False
rightOff = GPIO.setValue 80 False >> GPIO.setValue 81 False
rightReverse = GPIO.setValue 80 False >> GPIO.setValue 81 True


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


type ArmLevel = Int

setLeftArm :: ArmLevel -> IO ()
setLeftArm lev
  | lev ==  1 = PWM.setValue 1 20000000 320000
  | lev == -1 = PWM.setValue 1 20000000 2352000
  | otherwise = PWM.setValue 1 20000000 1300000
                 
setRightArm :: ArmLevel -> IO ()
setRightArm lev
  | lev ==  1 = PWM.setValue 2 20000000 2352000
  | lev == -1 = PWM.setValue 2 20000000 320000
  | otherwise = PWM.setValue 2 20000000 1300000
                

test1 = forward' 2000 >> stop' 2000 >> forward' 2000 >> backward' 2000
        >> forwardLeft' 2000 >> forwardRight' 2000
        >> backwardLeft' 2000 >> backwardRight' 2000
        >> turnLeft' 2000 >> turnRight' 2000 >> stop
        
test2 = return () || 2000 !! 2000 || 2000 && 2000 !| 2000 |! 2000 !& 2000 &! 2000 &| 2000 |& 2000 !! 0



