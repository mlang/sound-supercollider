{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main (main) where

import Text.Pretty.Simple (pPrintNoColor)
import           Data.Binary
import           Data.Foldable                (for_)
import           Data.Function                ((&))
import           Sound.SuperCollider.SynthDef
import           System.Process.Typed

build = proc "sclang" []
      & setStdin (byteStringInput "\"SynthDefs.scd\".load")
      & setStdout nullStream


main :: IO ()
main = do
  runProcess_ build
  putStrLn "{-# LANGUAGE OverloadedLists #-}"
  putStrLn "{-# LANGUAGE OverloadedStrings #-}"
  putStrLn "module Sound.SuperCollider.SynthDef.Builtin (SynthDef, def, playBuf) where"
  putStrLn ""
  putStrLn "import Sound.SuperCollider.SynthDef"
  putStrLn ""
  for_ [("def", "default"), ("playBuf", "PlayBuf")] $ \(fn, basename) -> do
    def :: SynthDef <- decodeFile $ basename ++ ".scsyndef"
    putStrLn $ fn ++ " :: SynthDef"
    putStrLn $ fn ++ " = " ++ show def
    putStrLn ""

