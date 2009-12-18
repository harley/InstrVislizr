--------------------------------------------------------------------------------
-- Author:   Harley Trung
-- Contact:  github.com/harleyttd
--           twitter.com/harleytt
-- Final Project, CPSC 432, Sound Representation & Synthesis, Fall 2009 at Yale
-- Revised date: December 15, 2009
--------------------------------------------------------------------------------
-- NOTE: Feel free to add more songs here, make sure to add an entry to songMap
module ScoreStore where

import Euterpea

-- must provide a SongMap in order to be used in the Song Select menu
songMapSample = [("c 5 qn", song1), ("c 4 hn", song2), ("c 5 qn :+: d 5 qn :+: e 5 qn", song3), ("Frere Jacques", fjSong),
           ("Fur Elise", furElise)]
song1 = c 5 qn
song2 = c 4 qn
song3 = c 5 qn :+: d 5 qn :+: e 5 qn

lmap f l = Euterpea.line (map f l)
--Frere Jacques
-- define hi, lo octave
lo, hi::Octave
lo = 4; hi = lo + 1
-- song:
frereJacques     = map (\x -> x lo qn) [Euterpea.f, g, a, Euterpea.f]
dorMezVous       = [a lo qn, bf lo qn, c hi hn]
sonNesLesMaTiNes = [c hi en, d hi en, c hi en, bf lo en, a lo qn, Euterpea.f lo qn]
dinDanDon        = [Euterpea.f lo qn, c lo qn, Euterpea.f lo hn]

fjSong :: Music Pitch
fjSong = line $ map (\x -> Euterpea.line (x++x)) [frereJacques, dorMezVous, sonNesLesMaTiNes, dinDanDon]

rh1 = line [e 6 en, ds 6 en, e 6 en, ds 6 en, e 6 en, b 5 en,
            d 6 en, c 6 en, a 5 qn]
rh2 = line [c 5 en, e 5 en, a 5 en, b 5 qn]
rh3 = line [e 5 en , gs 5 en, b 5 en, c 6 qn]
rh4 = line [e 5 en] :+: rh1
rh5 = line [e 5 en, c 6 en, b 5 en, a 5 hn]

rh  = rh1 :+: rest en :+: rh2 :+: rest en :+: rh3 :+:
       rest en :+: rh4 :+: rest en :+: rh2 :+: rest en :+: rh5

lh1 = line [a 3 en, e 4 en, a 4 en]
lh2 = line [e 4 en, gs 4 en, b 4 en]
lh3 = lh1 :+: rest dqn :+: lh2 :+: rest dqn :+: lh1
lh  = rest wn :+: lh3 :+: rest dqn :+: rest dhn :+: lh3

furElise = rh :=: lh

