{-# LANGUAGE Arrows #-}
module InstrStore where

import Euterpea
import Euterpea.Audio.Basics
import Euterpea.Audio.CSoundGenerators hiding (line)
import Euterpea.Audio.Types hiding (Signal)
import Euterpea.Audio.IO
import Euterpea.Audio.Render

instrMap :: InstrMap (Mono AudRate)
instrMap = [(myBass, plk), (myReed, reed)]

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

