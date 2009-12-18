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
--import Control.Concurrent (forkIO)
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
-- END OF would-have-been unnecessary piece of code
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- SADLY, this is OS-dependent. Linux is "play"
-- This is temporary, however, because playFile will eventually play from memory, not file
playerCall playerName fname = system (playerName ++ " " ++ fname)
-------------------------------------------------------------------------------

instrName :: InstrumentName -> String
instrName (Custom name) = name
instrName orig          = "modified-" ++ show orig

-- Work for both Mono and Stereo
render2samples instrMap song = uncurry toSamples $ renderSF song instrMap

countTrack :: forall a p. (AudioSample a, Clock p) => InstrMap (ASF.Signal p () a) -> Int
countTrack map = numChans (undefined :: a)

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

setPFields2 :: Signal Int -> Signal String -> UI (Signal Bool, Signal [Double])
setPFields2 i pfieldsStr = title "pFields" $ topDown $ do
    txt    <- textbox pfieldsStr
    leftRight $ do
        update <- button "Update on graph"
        save   <- smartButton "Write/append to file ---> " "Saving..."
        filename <- textbox (constant "pfields.txt")
        writePfields filename (snapshot_ (edge save) txt)
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

-- get current time to a string to create unique file names, if necessary
--getCurrentTime = getClockTime >>= toCalendarTime >>= return . calendarTimeToString

addPFields :: [Double] -> Music Pitch -> Music (Pitch, [NoteAttribute])
addPFields pf = mMap (\p -> (p, [PFields pf]))

zip3S = lift3 (,,)

-- Take advantage of Sound == IO () to inject IO operation
io2ui2 :: (a -> IO ()) -> Signal a -> UI ()
io2ui2 ioAction stuff = UI aux
  where aux ctx inp = (out <*> stuff <*> inp, (nullLayout, ()))
        out = pure (\x (ievt,s) -> ((nullGraphic, ioAction x),s))

vislizr :: forall a p. (AudioSample a, Clock p) => InstrMap (ASF.Signal p () a)
                 -> [(String, Music Pitch)]
                 -> [(InstrumentName, [Double])]
                 -> IO ()
vislizr = vislizrEx (1000, 700)
vislizrEx (width, height) instrMap songMap pFieldsMap =
    -- We are defining many utilities functions within this function to take
    -- advantage of the access to these arguments
    let pFieldsRealMap = Map.fromList pFieldsMap -- more efficient lookup
        -- =====================================================================
        getDefaultPfields i = case Map.lookup (fst (instrMap !! i)) pFieldsRealMap of
                                    Just lst -> lst
                                    Nothing -> []
        -- =====================================================================
        getSong :: Int -> Music Pitch
        getSong = snd . (songMap !!)
        -- Use a separate file for each song/instr
        getFileName :: (Int, Int) -> String
        getFileName (i,j) = let ins = instrName . fst $ instrMap !! i
                            in ins ++ "_" ++ (show j) ++ ".wav"
        -- =====================================================================
        selectInstr, selectSong :: UI (Signal Int)
        selectInstr = title "Select Instr instance" $ radio (map (instrName . fst) instrMap) 0
        selectSong  = title "Select Song" $ radio (map fst songMap) 0
        -- =====================================================================
        saveFile fname songs = io2ui2 writeOut (snapshot songs fname)
          where
            writeOut Nothing = return ()
            writeOut (Just (sngs, fileName)) = do
--                forkIO $ do -- caveat: does not work in ghci mode in windows (thus should comment out)
                    putStrLn $ "Saving... Please wait"
                    writeFile "./saving.tmp" "" -- create a tempfile to indicate saving is taking place

                    uncurry (outFile $ fileName) (renderSF sngs instrMap)
                    removeFile "./saving.tmp"   -- remove tempfile to indicate saving is done
                    putStrLn $ "wrote to file " ++ fileName
--                return ()
        -- =====================================================================
        playFile playerNameS fileNameES = io2ui2 playAux (snapshot fileNameES playerNameS)
          where
                playAux Nothing = return ()
                playAux (Just (fileName, playerName)) = do
--                  forkIO $ do
                    putStrLn $ "Started playing..." ++ fileName
                    x <- doesFileExist "./saving.tmp"
                    y <- doesFileExist $ fileName
                    if x then putStrLn "Cannot play file yet.  It seems file is still being written."
                         else if y then playerCall playerName fileName >> return () -- segfault in ghci (not when compiled)
                                   else putStrLn ("File " ++ fileName ++ " does not exist. Save it first.")
                    putStrLn "IF SEGFAULT, THIS IS NOT REACHED!"
--                  return ()
        -- =====================================================================
        waveVisualizer :: Signal (Int, Int, Bool) -> Signal [Double] -> UI ()
        waveVisualizer dirty plotData = title "Wave Visualizer" $ do
            xy <- leftRight $ do
                x <- titleNdisplay "Horizontal Zoom-Out:" $ hiSlider 1 (1, 100) 1
                y <- titleNdisplay "Vertical Zoom-In:" $ hSlider (1, 10) 1
                return (zipS x y)

            let events = unique (zipS dirty xy) =>> snd
            let fcanvas plotSamples = canvas (canvasWidth, canvasHeight) (snapshot events plotSamples =>> draw)

            if (countTrack instrMap == 2)
             then do fcanvas $ lift1 leftTrack plotData
                     fcanvas $ lift1 rightTrack plotData
             else fcanvas plotData
             where draw ((x,y), samples) = withColor Green (text (0,0) (show $ 1.0 / y)) //
                                           withColor Blue (plot (x,y) samples) //
                                           drawGrid
                   drawGrid :: Graphic
                   drawGrid = (polyline [(0, midY), (canvasWidth, midY)]) //
                              (polyline [(0,0), (canvasWidth, 0)])  //
                              (polyline [(0,canvasHeight), (canvasWidth, canvasHeight)])
                   -- note that coords is from top down instead of bottom up, hence the "midY -"
                   plot :: (Int, Double) -> [Double] -> Graphic
                   plot (scaleX, scaleY) lst = polyline $ zip [1,2..canvasWidth] (takeEvery scaleX ys)
                       where factor = amplifyY*scaleY
                             ys     = map ((midY-) . round . (*factor)) lst
                   -- dimension config
                   midY, canvasWidth, canvasHeight :: Int
                   canvasHeight = height `div` 5
                   canvasWidth = width
                   midY = canvasHeight `div` 2
                   amplifyY = fromIntegral $ canvasHeight `div` 2 -- amplify y-axis: [-1.0, 1] becomes [-100 to 100] pixel offsets

        render instrMap song = renderSF song instrMap
    in runUIEx (width, height) "InstrVislizr" $ do
        (i, j) <- leftRight $ do
            i       <- setSize (width `div` 2, height `div` 4) $ selectInstr
            j       <- setSize (width `div` 2, height `div` 4) $ selectSong
            return (i, j)
        (update, pFields) <- setPFields2 i (lift1 (show . getDefaultPfields) i)
        let
            -- signal values
            instr = lift1 (fst . (instrMap !!)) i
            origSong  = lift1 (snd . (songMap !!)) j
            song = lift3 f i j pFields
                where f ii jj pf = instrument (fst $ instrMap !! ii) (addPFields pf (getSong jj))

        waveVisualizer (zip3S i j update) (lift1 (render2samples instrMap) song)

        let ij = zipS i j
            events x = snapshot_ (edge x) song

        leftRight $ do
            let fileNameS = lift1 getFileName ij
            button "Save to file -->" >>= saveFile fileNameS . events
            display fileNameS
            -- TODO: implement play from memory instead of playing from file
            playerNameS <- textbox (constant "play")
            button "<-- Play file using" >>= \x -> playFile playerNameS (snapshot_ (events x) fileNameS)
            return ()

        return ()


-- A generic function to lift IO () operation to UI ()
-- It is not yet possible to do IO a to UI a however,
-- because this one injects the operation to Sound, which is IO ()
io2ui :: IO () -> UI ()
io2ui ioAction = UI aux
  where
    out = pure (\(ievt,s) -> ((nullGraphic, ioAction),s))
    aux ctx inp = (out <*> inp, (nullLayout, ()))

