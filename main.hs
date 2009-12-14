{-# LANGUAGE Arrows #-}
-- NOTE: to compile, ghc --make -i/home/dtt22/Euterpea/src/:/home/dtt22/Euterpea/dist/build -O3 main.hs

module Main where

{- TODO:
- http://github.com/willdonnelly/dyre: read haskell code on the fly
- plot table
- hindley milner: for writing an interpreter
- display vertical bar showing progress on plot while playing wave file
-}

import Euterpea
import Euterpea.UI
import Euterpea.UI.Widget
import Euterpea.UI.UIMonad
import Euterpea.UI.SOE hiding (line)
import Euterpea.UI.Signal
import Control.Applicative
import Euterpea.Audio.Render
import Euterpea.Audio.CSoundGenerators hiding (line)
import Euterpea.Audio.Basics
import Euterpea.Audio.Types hiding (Signal)
import Euterpea.Audio.IO
--import Time
import Directory (removeFile)
import System.Posix.Files (fileExist)
import Control.Concurrent (forkIO)
import System.Cmd (rawSystem)
import System.IO

import InstrStore
import NewWidgets
import ScoreStore
import Text.Printf

playerName = "play"

showS :: Show a => Signal a -> UI ()
showS = display . lift1 show

titleNdisplay :: (Show a) => String -> UI (Signal a) -> UI (Signal a)
titleNdisplay str action = title str $ leftRight $ action >>= \x -> showS x >> return x

-- add pFields to music
-- limitations: constant pFields for the whole music; any specific part can be overwritten
addPFields :: [Double] -> Music Pitch -> Music (Pitch, [NoteAttribute])
addPFields pf = mMap (\p -> (p, [PFields pf]))

getSong :: Int -> Music Pitch
getSong = snd . (songMap !!)

instrName :: InstrumentName -> String
instrName (Custom name) = name
instrName orig          = "modified-" ++ show orig

-- TODO: Work for both Mono and Stereo???
render (i, j) pFields = renderSF (instrument (fst $ instrMap !! i) (addPFields pFields (getSong j))) instrMap
render2samples i j pFields = uncurry toSamples $ render (i,j) pFields

--------------------------
-- BEGIN CONFIGURATIONS --
--------------------------
midY, canvasWidth, canvasHeight :: Int
canvasHeight = 200
canvasWidth = 1000
midY = canvasHeight `div` 2
amplifyY = 100 -- amplify y-axis: [-1.0, 1] becomes [-100 to 100] pixel offsets
--------------------------

selectInstr, selectSong :: UI (Signal Int)
selectInstr = title "Select Instr instance" $ radio (map (show . fst) instrMap) 0
selectSong  = title "Select Song" $ radio (map fst songMap) 0

listS :: [Signal a] -> Signal [a]
listS = Signal . zipL . (map unS)
    where zipL ([]:_) = []
          zipL lst    = map head lst : zipL (map tail lst)

setPFields2 :: Int -> [Double] -> UI (Signal Bool, Signal [Double])
setPFields2 n defaultPfields = title "pFields" $ topDown $ do
    lst <- (leftRight $ sequence $ reverse $ map f (pair n defaultPfields)) >>= return . listS
    display (lift1 unwords (lift1 (map dispDouble) lst))
    update <- button "Update"
    return (update, lst)
  where
    dispDouble f = printf "%.2f" (f::Double) :: String
    f (i, v)     = {-titleNdisplay ("p"++show i) $ -} hSlider (0, 10) v
    pair n []     = [(n,0)]
    pair 0 (p:ps) = [(n,p)]
    pair n (p:ps) = (n,p) : (pair (n-1) ps)

setPFields :: Int -> [Double] -> UI (Signal Bool, Signal [Double])
setPFields n defaultPfields = title "pFields" $ do
    txt    <- textbox (show defaultPfields)
    update <- button "Update"
    -- TODO: how to catch error here.  I want to reset list to original values if txt is an invalid text
    -- function "catch" requires IO to work, would require lifting to io
    -- but i can't lifft IO a -> UI a, I can only do IO () -> UI ()
    let lst = lift1 read txt
    return (update, lst)

zip3S = lift3 (,,)

main = runUIEx (canvasWidth+10, 900) "UI Demo" $ do
    (i, j, update, pFields) <- leftRight $ do
        i       <- selectInstr
        j       <- selectSong
        (update, fields)  <- setPFields 4 [0.2, 0.01, 0.5, 0.3]
        return (i, j, update, fields)

    let dirty = (zip3S i j update)

    waveVisualizer dirty (lift3 render2samples i j pFields)

    z <- smartButton "press me :-)" "zomg I am being pressed"

    leftRight $ do
        let events x = snapshot_ (edge x) (zipS i j)
        button "Save to file" >>= saveFile pFields . events
        -- TODO: implement play from memory instead of playing from file
        button "Play file" >>= playFile . events
    return ()

takeEvery :: Int -> [a] -> [a]
takeEvery n [] = []
takeEvery n (x:xs) = x : takeEvery n (drop (n-1) xs)

-- note that coords is from top down instead of bottom up, hence the "midY -"
plot :: (Int, Double) -> [Double] -> Graphic
plot (scaleX, scaleY) lst = polyline $ zip [1,2..canvasWidth] (takeEvery scaleX ys)
    where factor = amplifyY*scaleY
          ys     = map ((midY-) . round . (*factor)) lst

waveVisualizer :: Signal (Int, Int, Bool) -> Signal [Double] -> UI ()
waveVisualizer dirty plotData = title "Wave Visualizer" $ do
    xy <- leftRight $ do
        x <- titleNdisplay "Horizontal Zoom-Out:" $ hiSlider 1 (1, 100) 1
        y <- titleNdisplay "Vertical Zoom-In:" $ hSlider (1, 10) 1
        return (zipS x y)
    -- fstS comboS is included to convert to EventS
--    let events = snapshot (unique (zipS xy (fstS comboS)) =>> fst) (sndS comboS)
    let events = unique (zipS dirty xy) =>> snd
    canvas (canvasWidth, canvasHeight) (snapshot events plotData =>> draw)
          where
            draw ((x,y), samples) = withColor Green (text (0,0) (show $ 1.0 / y)) //
                                    withColor Blue (plot (x,2*y) samples) //
                                    drawGrid
            drawGrid :: Graphic
            drawGrid = (polyline [(0, midY), (canvasWidth, midY)]) //
                       (polyline [(0,0), (canvasWidth, 0)])  //
                       (polyline [(0,canvasHeight), (canvasWidth, canvasHeight)])

-- Use a separate file for each song/instr
getFileName :: (Int, Int) -> String
getFileName (i,j) = let ins = instrName . fst $ instrMap !! i
                    in ins ++ "_" ++ (show j) ++ ".wav"

-- get current time to a string to create unique file names, if necessary
--getCurrentTime = getClockTime >>= toCalendarTime >>= return . calendarTimeToString

saveFile :: Signal [Double] -> EventS (Int,Int) -> UI ()
saveFile pfields idxPair = UI aux
  where
    aux ctx inp = (out <*> pfields <*> idxPair <*> inp, (nullLayout, ()))
      where
        out = pure (\pf idx (ievt,s) -> ((nullGraphic, writeOut pf idx),s))
        writeOut _ Nothing = return ()
        writeOut pf (Just ij) = do
            -- forked threads will simply terminate when main thread terminates
            -- "daemonic threads"
            forkIO $ do
                putStrLn $ "Saving... Please wait"
                writeFile "./saving.tmp" "" -- create a tempfile to indicate saving is taking place
                let fileName = getFileName ij
                uncurry (outFile $ fileName) (render ij pf)
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
          forkIO $ do
            let fname = getFileName ij
            putStrLn $ "Started playing..." ++ fname
            x <- fileExist "./saving.tmp"
            y <- fileExist $ fname
            if x then putStrLn "Cannot play file yet.  It seems file is still being written."
                 else if y then rawSystem playerName [fname] >> return () -- TODO: why segfault after running this?
                           else putStrLn ("File " ++ fname ++ " does not exist. Save it first.")
            putStrLn "IF SEGFAULT, THIS IS NOT REACHED!"
          return ()

io2ui :: IO () -> UI ()
io2ui ioAction = UI aux
  where
    out = pure (\(ievt,s) -> ((nullGraphic, ioAction),s))
    aux ctx inp = (out <*> inp, (nullLayout, ()))

