{-# LANGUAGE Arrows, ScopedTypeVariables, NamedFieldPuns, FlexibleContexts #-}
--------------------------------------------------------------------------------
-- Author:   Harley Trung
-- Contact:  github.com/harleyttd
--           twitter.com/harleytt
-- Final Project, CPSC 432, Sound Representation & Synthesis, Fall 2009 at Yale
-- Revised date: December 15, 2009
--------------------------------------------------------------------------------
module InstrVislizr where
-- NOTE: to compile, ghc --make -i/home/dtt22/Euterpea/src/:/home/dtt22/Euterpea/dist/build -O3 main.hs

{- TODO:
- http://github.com/willdonnelly/dyre: read haskell code on the fly to use on InstrStore.hs and SongStore.hs
- plot table (genxx)
- display vertical bar showing progress on plot while playing wave file -- only after play from memory is implemented
-}

import Euterpea
import Euterpea.UI
import Euterpea.UI.Widget
import Euterpea.UI.UIMonad
import Euterpea.UI.SOE hiding (line)
import Euterpea.UI.Signal

import Control.Applicative

import qualified Euterpea.Audio.Types as ASF (Signal) -- for countChan
import Euterpea.Audio.Render
import Euterpea.Audio.CSoundGenerators hiding (line)
import Euterpea.Audio.Basics
import Euterpea.Audio.Types hiding (Signal)
import Euterpea.Audio.IO
--import Time
import System.Directory (doesFileExist, removeFile)
import Control.Concurrent (forkIO)
import System.Cmd (system)
import System.IO

import InstrStore
import NewWidgets
import ScoreStore
import Text.Printf

import Data.Map (Map)
import qualified Data.Map as Map

-------------------------------------------------------------------------------
-- NOTE: delete this block after toSampels id exported from  Euterpea/src/Euterpea/Audio/IO.hs
import Euterpea.Signal.SF (SF, unfold)
import Control.CCA.ArrowP (ArrowP, strip)
toSamples :: forall a p. (AudioSample a, Clock p) =>
             Double ->  ArrowP SF p () a -> [Double]
toSamples dur sf =
  let sr          = rate     (undefined :: p)
      numChannels = numChans (undefined :: a)
      numSamples  = truncate (dur * sr) * numChannels
  in take numSamples $ concatMap collapse $ unfold $ strip sf
-- END OF unnecessary piece of code
-------------------------------------------------------------------------------

pFieldsRealMap = Map.fromList pFieldsMap
getDefaultPfields i = case Map.lookup (fst (instrMap !! i)) pFieldsRealMap of
                        Just lst -> lst
                        Nothing -> []

-------------------------------------------------------------------------------
-- SADLY, this is OS-dependent. Linux is "play"
-- This is temporary, however, because playFile will eventually play from memory, not file
playerCall playerName fname = system (playerName ++ " " ++ fname)
-------------------------------------------------------------------------------

-- add pFields to music
-- limitations: constant pFields for the whole music; any specific part can be overwritten
addPFields :: [Double] -> Music Pitch -> Music (Pitch, [NoteAttribute])
addPFields pf = mMap (\p -> (p, [PFields pf]))

getSong :: Int -> Music Pitch
getSong = snd . (songMap !!)

instrName :: InstrumentName -> String
instrName (Custom name) = name
instrName orig          = "modified-" ++ show orig

-- Work for both Mono and Stereo
render (i, j) pFields = renderSF (instrument (fst $ instrMap !! i) (addPFields pFields (getSong j))) instrMap
render2samples i j pFields = uncurry toSamples $ render (i,j) pFields

countChan :: forall a p. (AudioSample a, Clock p) => InstrMap (ASF.Signal p () a) -> Int
countChan map = numChans (undefined :: a)

isStereo = countChan instrMap == 2

---------------------------------
-- GLOBAL CONFIGURATIONS (Ugly)--
---------------------------------
midY, canvasWidth, canvasHeight :: Int
canvasHeight = 150
canvasWidth = 1000
midY = canvasHeight `div` 2
amplifyY = 100 -- amplify y-axis: [-1.0, 1] becomes [-100 to 100] pixel offsets
---------------------------------

selectInstr, selectSong :: UI (Signal Int)
selectInstr = title "Select Instr instance" $ radio (map (instrName . fst) instrMap) 0
selectSong  = title "Select Song" $ radio (map fst songMap) 0

listS :: [Signal a] -> Signal [a]
listS = Signal . zipL . (map unS)
    where zipL ([]:_) = []
          zipL lst    = map head lst : zipL (map tail lst)

{-
-- Old code that use a bunch of sliders
setPFields :: Int -> [Double] -> UI (Signal Bool, Signal [Double])
setPFields n defaultPfields = title "pFields" $ topDown $ do
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
-}

setPFields2 :: Signal Int -> Int -> UI (Signal Bool, Signal [Double])
setPFields2 i n = title "pFields" $ do
    txt    <- textbox (lift1 (show . getDefaultPfields) i)
    update <- leftRight $ do
        ud <- button "Update on graph"
        save   <- smartButton "Write/append to file ---> " "Saving..."
        filename <- textbox (constant "pfields.txt")
        writePfields filename (snapshot_ (edge save) txt)
        return ud

    -- TODO: how to catch error here -- set original values if txt is an invalid text
    let lst = lift1 read txt
    -- function "catch" requires "IO a" to work, would require lifting to io
    -- but i can't lifft IO a -> UI a, I can only do IO () -> UI ()
    return (update, lst)

writePfields :: Signal String -> EventS String -> UI ()
writePfields fnameS pfieldsES = UI aux
  where
    aux ctx inp = (out <*> fnameS <*> pfieldsES <*> inp, (nullLayout, ()))
      where
        out = pure (\fname pfMb (ievt,s) -> ((nullGraphic, writeOut fname pfMb),s))
        writeOut _ Nothing = return ()
        writeOut fileName (Just pf) = do
            -- short file, no need forking
            appendFile fileName (pf++"\n")
            putStrLn $ "Wrote pfields to file " ++ fileName
            return ()

takeEvery :: Int -> [a] -> [a]
takeEvery n [] = []
takeEvery n (x:xs) = x : takeEvery n (drop (n-1) xs)

-- use drop 1 instead of tail to avoid exception error on empty list
leftTrack lst = takeEvery 2 lst
rightTrack lst= takeEvery 2 (drop 1 lst)

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
    let events = unique (zipS dirty xy) =>> snd
    let fcanvas plotSamples = canvas (canvasWidth, canvasHeight) (snapshot events plotSamples =>> draw)
    if isStereo
     then let plot1 = lift1 leftTrack plotData
              plot2 = lift1 rightTrack plotData
          in do
            fcanvas plot1
            fcanvas plot2
     else fcanvas plotData
     where draw ((x,y), samples) = withColor Green (text (0,0) (show $ 1.0 / y)) //
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

playFile :: Signal String -> EventS (Int,Int) -> UI ()
playFile playerNameS idxPair = UI aux
  where
    aux ctx inp = (out <*> playerNameS <*> idxPair <*> inp, (nullLayout, ()))
      where
        out = pure (\pname idx (ievt,s) -> ((nullGraphic, writeOut pname idx),s))
        writeOut _ Nothing = return ()
        writeOut playerName (Just ij) = do
          forkIO $ do
            let fname = getFileName ij
            putStrLn $ "Started playing..." ++ fname
            x <- doesFileExist "./saving.tmp"
            y <- doesFileExist $ fname
            if x then putStrLn "Cannot play file yet.  It seems file is still being written."
                 else if y then playerCall playerName fname >> return () -- TODO: why segfault after running this? (not segfault when compiled)
                           else putStrLn ("File " ++ fname ++ " does not exist. Save it first.")
            putStrLn "IF SEGFAULT, THIS IS NOT REACHED!"
          return ()

-- A generic function to lift IO () operation to UI ()
-- It is not yet possible to do IO a to UI a however,
-- because this one injects the operation to Sound, which is IO ()
io2ui :: IO () -> UI ()
io2ui ioAction = UI aux
  where
    out = pure (\(ievt,s) -> ((nullGraphic, ioAction),s))
    aux ctx inp = (out <*> inp, (nullLayout, ()))

