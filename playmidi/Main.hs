{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import           Control.Concurrent.STM          (atomically)
import           Control.Concurrent.STM.TMChan   (TMChan, readTMChan)
import           Control.Monad                   (join)
import           Control.Monad.IO.Class          (MonadIO (liftIO))
import           Data.Char                       (chr)
import qualified Sound.MIDI.File.Load            as MIDI
import           Sound.SuperCollider.Render      (play)
import           Sound.SuperCollider.Render.MIDI (midiFile)
import           Sound.SuperCollider.Server      (receiveSynthDef, startServer,
                                                  waitSilence, withServer)
import           Sound.SuperCollider.SynthDef    (defaultSynthDef)
import           System.Environment              (getArgs)
import           System.IO                       (BufferMode (NoBuffering),
                                                  hGetBuffering, hSetBuffering,
                                                  stdout)

main :: IO ()
main = getArgs >>= \case
  []   -> putStrLn "No file given"
  [mf] -> playmidi mf
  _    -> putStrLn "Too many arguments"

playmidi :: FilePath -> IO ()
playmidi fp = withServer (startServer 57110) $ do
  join $ receiveSynthDef defaultSynthDef
  silence <- waitSilence
  (p, ()) <- play 0.1 . midiFile =<< liftIO (MIDI.fromFile fp)
  showProgress =<< p
  silence

showProgress :: MonadIO m => TMChan a -> m ()
showProgress c = liftIO $ do
  bm <- hGetBuffering stdout
  hSetBuffering stdout NoBuffering
  putStr "Playing...."
  loop $ cycle "/|\\-"
  hSetBuffering stdout bm
 where
  loop (x:xs) = atomically (readTMChan c) >>= \case
    Just _ -> do
      putStr [chr 8, x]
      loop xs
    Nothing -> putStrLn $ [chr 8] ++ "Done"
