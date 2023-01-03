{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}
module Main (main) where

import Control.Monad.IO.Class (liftIO)
import           Control.Monad                    (join)
import           Data.Ratio                       ((%))
import qualified Sound.MIDI.File.Load             as Load
import           Sound.SuperCollider.Render.MIDI (midiFile)
import           Sound.SuperCollider.Message
import           Sound.SuperCollider.Render
import           Sound.SuperCollider.Server
import           Sound.SuperCollider.SynthDef
import           System.Environment               (getArgs)

main :: IO ()
main = getArgs >>= \case
  []   -> putStrLn "No file given"
  [mf] -> playmidi mf
  _    -> putStrLn "Too many arguments"

playmidi :: FilePath -> IO ()
playmidi fp = withServer (startServer 57110) $ do
  join $ receiveSynthDef defaultSynthDef
  silence <- waitSilence
  (tc, ()) <- play 0.1 . midiFile =<< liftIO (Load.fromFile fp)
  liftIO . logT =<< tc
  silence

