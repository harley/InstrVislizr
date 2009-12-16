{-# LANGUAGE Arrows, ScopedTypeVariables, NamedFieldPuns, FlexibleContexts #-}
-- NOTE: to compile, type: make

module Main where
--------------------------------------------------------------------------------
-- Author:   Harley Trung
-- Contact:  github.com/harleyttd
--           twitter.com/harleytt
-- Final Project, CPSC 432, Sound Representation & Synthesis, Fall 2009 at Yale
-- Revised date: December 15, 2009
--------------------------------------------------------------------------------

import InstrVislizr
import Euterpea.UI
import Euterpea.UI.Signal
import NewWidgets

zip3S = lift3 (,,)

main = runUIEx (canvasWidth+10, 900) "UI Demo" $ do
    (i, j) <- leftRight $ do
        i       <- selectInstr
        j       <- selectSong
        return (i, j)
    (update, pFields) <- setPFields2 i 4
    let dirty = (zip3S i j update)

    waveVisualizer dirty (lift3 render2samples i j pFields)
    --z <- smartButton "press me :-)" "zomg I am being pressed"

    leftRight $ do
        let events x = snapshot_ (edge x) (zipS i j)
        button "Save to file" >>= saveFile pFields . events
        -- TODO: implement play from memory instead of playing from file
        playerName <- textbox (constant "play")
        button "<-- Play file using" >>= playFile playerName . events
    return ()

