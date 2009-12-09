module ScoreStore where

import Euterpea

-- must provide a SongMap in order to be used in the Song Select menu

songMap = [("c 5 qn", song1), ("c 4 hn", song2), ("c 5 qn :+: d 5 qn :+: e 5 qn", song3)]
song1 = c 5 qn
song2 = c 4 qn
song3 = c 5 qn :+: d 5 qn :+: e 5 qn

