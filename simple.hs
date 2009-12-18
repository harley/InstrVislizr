{-# LANGUAGE Arrows #-}

import Euterpea
import Euterpea.Audio.CSoundGenerators
import Euterpea.Audio.Types
import Euterpea.Audio.Render
import Euterpea.Audio.Basics
import Euterpea.Audio.IO

import InstrVislizr
import ScoreStore (songMapSample)
import InstrStore hiding (f1)

f1 = gen10 4096 [1]
simple :: Instr (Mono AudRate)
simple dur pch vol pFields | length pFields > 10 =
    let amps = take 5 pFields
        durations = take 5 (drop 5 pFields)
        f d = d / sum durations * fromRational dur
     in proc _ -> do
       s   <- oscil f1 0                    -< apToHz pch
       amp <- linseg amps (map f durations) -< ()
       returnA -< s * amp * 0.2
simple dur pch vol _ | otherwise =
    simple dur pch vol [1,0.99,0.4,0.2,0,0,      -- first 5 values (amp)
                        0.02, 0.38, 0.4, 0.2, 0, -- next 5 values (dur)
                        0.2]                     -- final scale factor
simpleSong = a 4 qn

simpleInstr = Custom "simple"
instrMap = [(simpleInstr, simple)]
songMap = [("Simple Song", simpleSong)]
pFieldsMap = [(simpleInstr, [1,0.99,0.4,0.2,0,0, 0.02, 0.38, 0.4, 0.2, 0, 0.2])]

main = vislizr instrMapStereo songMapSample pFieldsMapSample

