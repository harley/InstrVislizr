{-# LANGUAGE Arrows, ScopedTypeVariables, NamedFieldPuns, FlexibleContexts #-}
-- THIS IS A DEMO file.
-- The main library is in InstrVislizr.hs
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
import InstrStore
import ScoreStore

-- add pFields to music
-- limitations: constant pFields for the whole music; any specific part can be overwritten
main = vislizr instrMapStereo songMapSample pFieldsMapSample

