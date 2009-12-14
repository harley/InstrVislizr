{-# LANGUAGE Arrows #-}
module InstrStore where

import Euterpea
import qualified Euterpea (f, line)
import Euterpea.Audio.Basics
import Euterpea.Audio.CSoundGenerators hiding (line)
import Euterpea.Audio.Types hiding (Signal)
import Euterpea.Audio.IO
import Euterpea.Audio.Render
import Control.Arrow ((>>>), (^>>), arr)
import Euterpea.Audio.Types
import Euterpea hiding (line, delay, f)


--instrMap :: InstrMap (Mono AudRate)
instrMap = [(myBass, plk), (myReed, reed), (thornyMessy, thorny2), (tipsyFrenzy, tipsy2)]
--instrMap = [(tipsyFrenzy, tipsy)]
instrMapStereo = [(thornyMessy, thorny), (tipsyFrenzy, tipsy)]

saw = gen10 4096 [1, 0.5, 0.333, 0.25, 0.2, 0.166, 0.142, 0.125,
                  0.111, 0.1, 0.09, 0.083, 0.076, 0.071, 0.066, 0.062]

myBass, myReed :: InstrumentName
myBass = Custom "pluck-like"
myReed = Custom "reed-like"

-- TODO: change to polymorphic to allow stereo as well
type InstrPair = (InstrumentName, InstrType)
type InstrType = Instr (Mono AudRate)

plk :: InstrType
plk dur pch vol pfields =
    let vel = fromIntegral vol / 127 / 3
        freq = apToHz pch
        sf = pluck saw freq SimpleAveraging
    in proc _ -> do
         a <- sf -< freq
         returnA -< a * vel

reedyWav = compSine1 1024 [0.4, 0.3, 0.35, 0.5, 0.1, 0.2, 0.15,
                           0.0, 0.02, 0.05, 0.03]
reed :: Instr (Mono AudRate)
reed dur pch vol pfields =
    let reedy = oscil reedyWav 0
        freq = apToHz pch
        vel = fromIntegral vol / 127 / 3
        env = linseg [0, 1, 0.8, 0.6, 0.7, 0.6, 0]
                     (replicate 6 (fromRational dur/6))
    in proc _ -> do
      amp <- env -< ()
      r1 <- reedy -< freq
      r2 <- reedy -< freq + (0.023 * freq)
      r3 <- reedy -< freq + (0.019 * freq)
      let [a1, a2, a3] = map (* (amp * vel)) [r1, r2, r3]
      let rleft = a1 * 0.5 + a2 * 0.44 * 0.35 + a3 * 0.26 * 0.65
          rright = a1 * 0.5 + a2 * 0.44 * 0.65 + a3 * 0.26 * 0.35
      returnA -< rleft* 2

-------------
--PROBLEM 1--
-------------
type AudSF a b = Signal AudRate a b

myF = gen09 1024 [(0.5, 1, 55), (1.3, 2.3, 40), (2.6, 1.32, 35), (3.45, 3, 20)]

thorny :: Instr (Stereo AudRate)
thorny dur pch vol (x:y:z:_) =
    let freq  = apToHz pch
        vel   = fromIntegral vol / 127 / 3
        env   = linseg [0, 1, 0.8, 0.6, 0.7, 0.6, 0] (map (\x->fromRational dur/x) [1..6])
        recur = oscil myF 0
    in proc _ -> do
      amp <- env -< ()
      r1 <- recur -< freq
      r2 <- recur -< (1+x) * freq
      r3 <- recur -< (1+y) * freq
      let rleft = sum $ zipWith3 (\a b c -> a*b*c) [x, y, z] [r1, r2, r3] [amp, amp*vel, amp*vel]
          rright = sum $ zipWith3 (\a b c -> a*b*c) [z, y, x] [r1, r2, r3] [amp*vel, amp, amp*vel]
      returnA -< (rleft, rright)

f2 = gen10 4096 [1, 0.5, 0.333, 0.25, 0.2, 0.166, 0.142, 0.125,
                  0.111, 0.1, 0.09, 0.083, 0.076, 0.071, 0.066, 0.062]

tipsy :: Instr (Stereo AudRate)
tipsy dur pch vol [] =
    let vel = fromIntegral vol / 127 / 3
        freq = apToHz pch
        sf = oscil saw 0
    in proc _ -> do
         a <- sf -< freq
         returnA -< (a * vel * 0.4, a * vel * 0.6)
tipsy dur pch vol (x:xs) =
    let vel = fromIntegral vol / 127 / 3 / x
        freq = apToHz pch
    in proc _ -> do
        a1 <- oscil f2 0 -< 3200
        a2 <- oscil f2 0 -< 1
        let del = (a2*0.99 + 1)  -- use a LFO to modulate the delay tap
        a <- vdelay 3 -< (a1*0.9, del)
        (b1, b2) <- tipsy dur pch vol xs -< ()
        returnA -< ((1-a/2)*b2*a2, (1-a)*b2)

tipsy2 dur pch vol pfields = proc _ -> do
    xy <- tipsy dur pch vol pfields -< ()
    returnA -< (fst xy)

thorny2 dur pch vol pfields = proc _ -> do
    xy <- thorny dur pch vol pfields -< ()
    returnA -< (fst xy)

-------------
--PROBLEM 2--
-------------

-- test instrument
thornyMessy :: InstrumentName
thornyMessy = Custom "thorny"
tipsyFrenzy :: InstrumentName
tipsyFrenzy = Custom "tipsy"

