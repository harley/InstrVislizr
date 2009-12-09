{-# LANGUAGE Arrows #-}
module InstrVisualizr where

import Euterpea
import Euterpea.UI
import Euterpea.UI.Widget
import Euterpea.UI.UIMonad
import Euterpea.UI.SOE hiding (line)
import Euterpea.UI.Signal
import Control.Applicative
import Numeric
import Euterpea.Audio.Render
import Euterpea.Audio.CSoundGenerators hiding (line)
import Euterpea.Audio.Basics
import Euterpea.Audio.Types hiding (Signal)
import Euterpea.Audio.IO
import Time
import Directory (removeFile)
import System.Posix.Files (fileExist)
import Control.Concurrent (forkIO)
import System (system)

import InstrStore
import NewWidgets
import ScoreStore

playerName = "ls"

display' :: Show a => Signal a -> UI ()
display' = display . lift1 show

titleNdisplay :: (Show a) => String -> UI (Signal a) -> UI (Signal a)
titleNdisplay str action = title str $ leftRight action >>= \x -> display' x >> return x

-- add pFields to music
-- limitations: constant pFields for the whole music; any specific part can be overwritten
addPFields :: [Double] -> Music Pitch -> Music (Pitch, [NoteAttribute])
addPFields pf = mMap (\p -> (p, [PFields pf]))

-- TODO: should also accept Music (Pitch, [NoteAttribute]) ?
--getSong :: Int -> Music Pitch
getSong = snd . (songMap !!)

render :: (Int, Int) -> (Int, (Double, Mono AudRate))
render (i, j) = (i+j, -- used to detect signal changes in instrument/song selection
                 renderSF (instrument (fst $ instrMap !! i) (addPFields [1,2,3] (getSong j)))
                           instrMap)

midY, canvasWidth, canvasHeight :: Int
canvasHeight = 400
canvasWidth = 1000
midY = canvasHeight `div` 2
amplifyY = 100 -- amplify y-axis: [-1.0, 1] becomes [-100 to 100] pixel offsets

selectInstr, selectSong :: UI (Signal Int)
selectInstr = title "Select Instr instance" $ radio (map (show . fst) instrMap) 0
selectSong  = title "Select Song" $ radio (map fst songMap) 0

setPFields :: UI ([Signal Double])
setPFields = title "pFields" $ do
    p1 <- titleNdisplay "p1" $ hSlider (0, 100) 0
    p2 <- titleNdisplay "p2" $ hSlider (0, 100) 0
    p3 <- titleNdisplay "p3" $ hSlider (0, 100) 0
    return [p1, p2, p3]

main = runUIEx (canvasWidth+10,900) "UI Demo" $ do
    (idxPair, pFields) <- leftRight $ do
        i       <- selectInstr
        j       <- selectSong
        fields  <- setPFields
        return (zipS i j, fields)

    waveVisualizer (lift1 render idxPair)

    z <- smartButton "Currently unpressed" "zomg I am being pressed"

    leftRight $ do
        let events x = snapshot_ (edge x) idxPair
        button "Save to file" >>= saveFile . events
        -- TODO: implement play from memory instead of playing from file
        button "Play file" >>= playFile . events
    return ()

takeEvery :: Int -> [a] -> [a]
takeEvery n [] = []
takeEvery n (x:xs) = x : takeEvery n (drop (n-1) xs)

-- note that coords is from top down instead of bottom up, hence the "midY -"
plot :: (Int, Double) -> [Double] -> Graphic
plot (scaleX, scaleY) lst = polyline $ zip [1,2..canvasWidth] (takeEvery scaleX ys)
    where  ys = map ((midY-) . round . (*(amplifyY*scaleY))) lst

waveVisualizer :: Signal (Int, (Double, Mono AudRate)) -> UI ()
waveVisualizer comboS = title "Wave Visualizer" $ do
    xy <- leftRight $ do
        x <- titleNdisplay "Horizontal Zoom-Out:" $ hiSlider 1 (1, 100) 1
        y <- titleNdisplay "Vertical Zoon-In:" $ hSlider (1, 10) 1
        return (zipS x y)
    -- Using fstS and sndS comboS because unique cannot take signal of arrow
    let events = snapshot (unique (zipS xy (fstS comboS)) =>> fst) (sndS comboS)
    canvas (canvasWidth, canvasHeight) (events  =>> draw)
--        canvas (0,0) (unique y =>> \y -> text(0,0) (show $ 1.0 / y))
          where
            samples sf = uncurry toSamples sf
            draw ((x,y),audsf)  = withColor Green (text (0,0) (show $ 1.0 / y)) //
                                  withColor Blue (plot (x,2*y) (samples audsf)) //
                                  drawGrid
            drawGrid :: Graphic
            drawGrid = (polyline [(0, midY), (canvasWidth, midY)]) //
                       (polyline [(0,0), (canvasWidth, 0)])  //
                       (polyline [(0,canvasHeight), (canvasWidth, canvasHeight)])

-- we can easily change getFileName to use a separate file for each song/instr
getFileName = const "test.wav"
-- get current time to a string to create unique file names, if necessary
--getCurrentTime = getClockTime >>= toCalendarTime >>= return . calendarTimeToString

saveFile :: EventS (Int,Int) -> UI ()
saveFile idxPair = UI aux
  where
    aux ctx inp = (out <*> idxPair <*> inp, (nullLayout, ()))
      where
        out = pure (\idx (ievt,s) -> ((nullGraphic, writeOut idx),s))
        writeOut Nothing = return ()
        writeOut (Just ij) = do
            -- forked threads will simply terminate when main thread terminates
            -- "daemonic threads"
            forkIO $ do
                putStrLn $ "Saving... Please wait"
                writeFile "./saving.tmp" "" -- create a tempfile to indicate saving is taking place
                let rendered = snd $ render ij
                let fileName = getFileName ij
                uncurry (outFile $ fileName) rendered
                putStrLn $ "wrote to file " ++ fileName
                removeFile "./saving.tmp"   -- remove tempfile to indicate saving is done
            return ()

playFile :: EventS (Int,Int) -> UI ()
playFile idxPair = UI aux
  where
    aux ctx inp = (out <*> idxPair <*> inp, (nullLayout, ()))
      where
        out = pure (\idx (ievt,s) -> ((nullGraphic, writeOut idx),s))
        writeOut Nothing = return ()
        writeOut (Just ij) = do
            x <- fileExist "./saving.tmp"
            if x then putStrLn "Cannot play file yet.  It seems file is still being written."
             else system (playerName ++ " " ++ getFileName ij) >> return () -- TODO: why segfault after running this?




{-
vol :: Integer -> Music Pitch -> Music (Pitch, [NoteAttribute])
vol v (Prim (Note d p)) = Prim (Note d (p, [Volume v]))
vol v (Prim (Rest d)) = Prim (Rest d)
vol v (a :+: b) = vol v a :+: vol v b
vol v (a :=: b) = vol v a :=: vol v b
vol v (Modify c m) = Modify c (vol v m)





-}

