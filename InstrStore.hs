{-# LANGUAGE Arrows #-}
--------------------------------------------------------------------------------
-- Author:   Harley Trung
-- Contact:  github.com/harleyttd
--           twitter.com/harleytt
-- Final Project, CPSC 432, Sound Representation & Synthesis, Fall 2009 at Yale
-- Revised date: December 15, 2009
--------------------------------------------------------------------------------
-- NOTE: Feel free to add more instruments here, make sure to instrMap
module InstrStore where

import qualified Euterpea (f, line)
import Euterpea.Audio.Basics
import Euterpea.Audio.CSoundGenerators hiding (line)
import Euterpea.Audio.Types hiding (Signal)
import Euterpea.Audio.IO
import Euterpea.Audio.Render
import Control.Arrow ((>>>), (^>>), arr)
import Euterpea.Audio.Types
import Euterpea hiding (line, delay, f)

pFieldsMap = [(thornyMessy, [0.2, 0.05, 0.5, 0.3]), (ericFlute, [1.0])]
instrMap   = instrMapStereo

-- TODO: Can we have a mixed instrMap of both Mono and Stereo?
--instrMapMono ::  (Clock p, AudioSample a) => InstrMap ( Signal p () a)
instrMapMono :: InstrMap (Mono AudRate)
instrMapMono = [(myBass, plk), (myReed, reed), (thornyMessy, thorny1), (tipsyFrenzy, tipsy1),
                (ericFlute, flute)
                ]

instrMapStereo :: InstrMap (Stereo AudRate)
instrMapStereo = [(thornyMessy, thorny), (tipsyFrenzy, tipsy), (i1104, instr1104)]

saw = gen10 4096 [1, 0.5, 0.333, 0.25, 0.2, 0.166, 0.142, 0.125,
                  0.111, 0.1, 0.09, 0.083, 0.076, 0.071, 0.066, 0.062]

myBass, myReed :: InstrumentName
myBass = Custom "pluck-like"
myReed = Custom "reed-like"

-- Some common shorthands
type AudSF a b = Signal AudRate a b
type CtrSF a b = Signal CtrRate a b
constA :: Clock p => a -> Signal p () a
constA = arr . const

-- INSTRUMENTS BEGEIN HERE
--plk :: Instr (Mono AudRate)
plk dur pch vol pfields =
    let vel = fromIntegral vol / 127 / 3
        freq = apToHz pch
        sf = pluck saw freq SimpleAveraging
    in proc _ -> do
         a <- sf -< freq
         returnA -< a * vel

reedyWav = compSine1 1024 [0.4, 0.3, 0.35, 0.5, 0.1, 0.2, 0.15,
                           0.0, 0.02, 0.05, 0.03]
--reed :: Instr (Mono AudRate)
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

--------------
-- From HW4 --
--------------
-------------
--PROBLEM 1--
-------------
myF = gen09 1024 [(0.5, 1, 55), (1.3, 2.3, 40), (2.6, 1.32, 35), (3.45, 3, 20)]

--thorny :: Instr (Stereo AudRate)
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

--tipsy :: Instr (Stereo AudRate)
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

tipsy1 dur pch vol pfields = proc _ -> do
    xy <- tipsy dur pch vol pfields -< ()
    returnA -< (fst xy)

thorny1 dur pch vol pfields = proc _ -> do
    xy <- thorny dur pch vol pfields -< ()
    returnA -< (fst xy)

thornyMessy :: InstrumentName
thornyMessy = Custom "thorny"
tipsyFrenzy :: InstrumentName
tipsyFrenzy = Custom "tipsy"

--------------
-- From HW4 --
--------------
normalize :: [Double] -> [Double]
normalize z = map (/ (maximum z)) z

f :: Double -> Table
--SINEWAVE
f 1  = gen10 8192 [1]
--FIRST COMPONENT
f 11 = gen09 1024 [(0.5, 1, 55    ), (1.3, 2.3, 40  ), (2.6, 1.32, 35    ), (3.45, 3, 20)]
f 12 = gen09 1024 [(1, 1, 90      ), (1.3, 2.3, 84  ), (1.6, 1.32, 75    ), (2.45, 3, 60)]
--SECOND COMPONENT
f 21 = gen09 1024 [(0.7, 3, 66    ), (1.63, 2.2, 37 ), (3.36, 4.32, 16   ), (1.45, 3, 12)]
f 22 = gen09 1024 [(1, 1, 10      ), (2.6, 1.13, 84 ), (0.8, 2.46, 75    ), (4.9, 1.5, 60)]
--THIRD COMPONENT
f 31 = gen09 1024 [(2.1, 1, 22    ), (4.18, 6.6, 111), (1.12, 1.1, 5     ), (5.15, 9, 36)]
f 32 = gen09 1024 [(1, 1, 79      ), (2.6, 1.13, 84 ), (0.8, 2.46, 75    ), (4.9, 1.5, 60)]
--FOURTH COMPONENT
f 41 = gen09 1024 [(6.62, 2.5, 44 ), (7.1, 2.2, 48  ), (2.89, 3.5, 1)]
f 42 = gen09 1024 [(0.2, 1.6, 179 ), (2.55, 1.2, 4  ), (0.16, 4.12, 123)]
--FIFTH COMPONENT
f 51 = gen09 1024 [(8.4, 0.25, 188), (1.02, 9.9, 8  ), (4.48, 4.4, 100   ), (1.37, 2.25, 90)]
f 52 = gen09 1024 [(0.25, 4, 79   ), (5.4, 0.42, 184), (0.8, 9.12, 57    ), (1.24, 6, 6)]
--SIXTH COMPONENT
f 61 = gen09 1024 [(1.62, 0.25, 8 ), (5.1, 9.9, 180 ), (0.89, 4.4, 1     ), (3, 2.4, 30       ), (6.85, 2.25, 270)]
f 62 = gen09 1024 [(1.25, 4, 79   ), (2.55, 2.1, 48 ), (0.16, 9.12, 37   ), (2.4, 3, 7        ), (5.96, 6, 160)]
--PANNING FUNCTIONS
f 71 = gen10 1024 [1, 1, 1, 1, 1, 1, 1, 1, 1, 1]
f 72 = gen10 1024 [0, 0, 0, 1, 0, 1, 0, 1, 0, 1]
f 73 = gen09 1024 [(1.5, 0.8, 180 ), (2.4, 0.78, 170), (3.5, 0.8, 160    ), (4.6, 0.65, 140)]
f 74 = gen09 1024 [(1.3, 0.5, 170 ), (3.34, 0.6, 190), (4.7, 0.7, 140    ), (5.33, 0.35, 200)]
f _  = error "Table not exist"

--instr1104 :: Instr (Stereo AudRate)
instr1104 dur ap vol pFields =
   let  p n     = pFields !! (n-3)
        idur    = p 3
        iamp    = p 4
        ifreq   = p 5
        iatt    = p 6
        idec    = p 7
        imaxaf  = p 8 / 100.0
        imaxff  = p 9 / 100.0
        irs2    = sqrt 2.0
        isr2b2  = irs2 / 2.0
        imaxpan = 2
        ipanfunc= p 34
        component j = let irampi  = p j
                          imaxafi = irampi * imaxaf
                          iafunci = p (j+2)
                          ifreqi  = p (j+1) * ifreq
                          imaxffi = ifreqi * imaxff
                          iffunci = p (j+3)
                      in proc _ -> do
                         kampfi  <- oscil1 (f iafunci) 0 idur -< imaxafi
                         kfreqfi <- oscil1 (f iffunci) 0 idur -< imaxffi
                         ai'     <- oscili (f 1) 0            -< ifreqi+kfreqfi
                         returnA -< ai' * (irampi+kampfi)
        choose kp list | kp < -1    = list !! 0
                       | kp >  1    = list !! 2
                       | otherwise  = list !! 1
   in proc _ -> do
     kenv <- linen iatt idur idec -< iamp
     kpan <- oscil1 (f ipanfunc) 1 idur -< imaxpan
     let ktemp   = sqrt (1+kpan*kpan)
     let kpleft  = choose kpan [2.0/(1+kpan*kpan), isr2b2*(1-kpan)/ktemp, 0                 ]
     let kpright = choose kpan [0                , isr2b2*(1+kpan)/ktemp, 2.0/(1+kpan*kpan) ]
     a1 <- component 10 -< ()
     a2 <- component 14 -< ()
     a3 <- component 18 -< ()
     a4 <- component 22 -< ()
     a5 <- component 26 -< ()
     a6 <- component 30 -< ()
     let iampsum = sum (map p [10, 14, 18, 22, 26, 30])
     let asig    = kenv*(a1+a2+a3+a4+a5+a6) / iampsum / 32767
     returnA -< (kpleft*asig, kpright*asig)

i1104pFields :: [Double]
i1104pFields = [0    , 5   , 7000 , 100 , 1   , 1.5 , 20, 5 , 1   , 1, 11, 12, 0.86, 2.01, 21, 22, 0.77, 3.02, 31, 32, 0.68, 4.03, 41, 42, 0.59, 5.04, 51, 52, 0.5 , 6.05, 61, 62, 11]

i1104 :: InstrumentName
i1104 = Custom "1104"
--------------
-- From HW5 --
--------------
f1 = gen10 8192 [1]

vibrato :: Double -> Double -> AudSF Double Double
vibrato vfrq dep = proc afrq -> do
  vib <- oscil f1 0 -< vfrq
  aud <- oscil f1 0 -< afrq + vib * dep
  returnA -< aud

--ginstar :: Instr (AudSF () (Double, Double))
ginstar dur pch vol pfields =
    let sr = rate (undefined :: AudRate)
        vel = fromIntegral vol / 127 * 2
        buzzMe = buzz f1 0
        freq   = apToHz pch
        nHarms = round (sr/2/freq)
        fdur = fromRational dur
        env = expseg [1, 0.4, 0.7] (map (* fdur) [0.7, 0.1, 0.2])
    in proc _ -> do
        amp     <- env -< ()
        slopDown <- expon 20000 fdur 20 -< 1
        sig     <- buzzMe -< (freq, nHarms)
        s1      <- oscil f1 0 -< 440
        sine    <- oscil f1 0 -< s1
        f       <-constA 110 -< ()
        vib     <- vibrato 2 5 -< f
        a <- butterlp -< (sig * amp, slopDown)
        returnA -< (a * vel * 0.45 * vib, a * vel * 0.55)

-- From Eric's lecture
flute :: Instr (AudSF () Double)
flute durR pch vol (lfofreq:_) =
    let press = 0.93
        breath = 0.02
        freq   = apToHz pch
        dur    = fromRational durR
        amp    = fromIntegral vol / 127 * 0.35
        env1    = linseg [0, 1.1*press, press, press*0.9, 0]
                         [0.06, 0.2, dur-0.31, 0.05]
        env2    = linseg [0, 1, 1, 0] [0.01, dur-0.02, 0.01]
        envibr  = linseg [0, 0, 1, 1] [0.5, 0.5, dur-1]
    in proc _ -> do
        amp1 <- env1 -< ()
        amp2 <- env2 -< ()
        ampv <- envibr -< ()
        flow <- rand 1 -< amp1
        vibr <- oscils lfofreq -< 0.1 * ampv
        rec
          let feedbk = body * 0.4
          body <- delay (1/freq)   -< out
          x    <- delay (1/freq/2) -< breath*flow +amp1+vibr+feedbk
          out  <- tone             -< (x - x*x*x + feedbk, 2000)
        returnA -< out * amp * amp2

ericFlute = Custom "Eric's flute"

