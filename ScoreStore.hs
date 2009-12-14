module ScoreStore where

import Euterpea

-- must provide a SongMap in order to be used in the Song Select menu

songMap = [("c 5 qn", song1), ("c 4 hn", song2), ("c 5 qn :+: d 5 qn :+: e 5 qn", song3)]
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
fjSong = Euterpea.line $ map (\x -> Euterpea.line (x++x)) [frereJacques, dorMezVous, sonNesLesMaTiNes, dinDanDon]

