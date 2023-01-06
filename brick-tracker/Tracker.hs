{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
module Tracker (tracker, AppState) where

import           Brick.AttrMap
import           Brick.BChan
import           Brick.Main
import           Brick.Types
import           Brick.Widgets.Core            (fill, raw, txt, (<=>))
import           Control.Concurrent            (ThreadId, forkIO)
import           Control.Concurrent.STM (atomically)
import           Control.Concurrent.STM.TChan (readTChan)
import           Control.Concurrent.STM.TMChan (TMChan)
import           Control.Monad                 (forever, join, void)
import           Control.Monad.Catch           (MonadMask)
import           Control.Monad.IO.Class
import           Data.Text                     (Text, pack)
import           Data.Text.IO                  (hGetLine)
import           Graphics.Vty                  as Vty
import           Lens.Micro                    ((^.))
import           Lens.Micro.Mtl
import           Lens.Micro.TH
import           Sound.SuperCollider.Message   (Message, AddAction (AddToTail))
import           Sound.SuperCollider.Render
import           Sound.SuperCollider.Server
import           Sound.SuperCollider.SynthDef
import           System.IO                     (Handle)
import           System.IO.Error               (catchIOError, isEOFError)

data AppEvent = SynthPost Text
              | OSC Message
              deriving (Eq, Read, Show)

data AppState = State {
  _synthesizer :: Server
, _synthOutput :: [Text]
}

makeLenses ''AppState

appState :: Server -> AppState
appState s = State s []

instance HasSuperCollider AppState where
  supercollider = synthesizer

sound x = (`onServer` x) =<< use supercollider

type WidgetName = ()

type Tracker = App AppState AppEvent WidgetName
type AppM = EventM WidgetName AppState

buildVty :: IO Vty
buildVty = Vty.mkVty Vty.defaultConfig

welcome :: RenderT (ServerT AppM) ()
welcome = void $ gated 1 $ newSynth AddToTail "default" [("freq", 440)]

tracker :: (MonadIO m, MonadMask m) => m AppState
tracker = withServer (startServer 57110) $ do
  (ha, hb, hc, hd) <- viewServer handles
  mq <- messages
  c <- liftIO $ newBChan 128
  void . liftIO . forkIO . forever $
    writeBChan c . OSC =<< atomically (readTChan mq)
  void $ readHandle SynthPost c ha
  void $ readHandle SynthPost c hb
  void $ readHandle SynthPost c hc
  void $ readHandle SynthPost c hd
  catchup <- receiveSynthDef defaultSynthDef
  vty <- liftIO buildVty
  catchup
  liftIO . customMain vty buildVty (Just c) app . appState =<< viewServer id
 where
  app = App { .. } where
    appStartEvent = sound $ do
      play_ 0.05 welcome
    appDraw s = [synthLog] where
      synthLog = renderBottomUp $ txt <$> s ^. synthOutput
    appHandleEvent = \case
      AppEvent (SynthPost x) -> synthOutput %= (x :)
      VtyEvent (Vty.EvKey (Vty.KChar 'q') [Vty.MCtrl]) -> halt
      x -> synthOutput %= (pack (show x) :)
    appChooseCursor = showFirstCursor
    appAttrMap = const $ attrMap Vty.defAttr []

play_ l x = do
  (_ :: AppM (TMChan Rational), ()) <- play l x
  pure ()

readHandle :: MonadIO m => (Text -> e) -> BChan e -> Handle -> m ThreadId
readHandle c mq h = liftIO . forkIO . eof (const $ pure ()) . forever $
  writeBChan mq . c =<< hGetLine h

eof :: (IOError -> IO a) -> IO a -> IO a
eof x f = catchIOError f $ \e -> (if isEOFError e then x else ioError) e

renderBottomUp :: [Widget n] -> Widget n
renderBottomUp ws = Widget Greedy Greedy $
    render . (fill ' ' <=>) . raw =<< go ws . availHeight =<< getContext
  where
    go [] _ = pure Vty.emptyImage
    go (c:cs) remainingHeight = do
        img <- image <$> render c
        let newRemainingHeight = remainingHeight - Vty.imageHeight img
        case compare newRemainingHeight 0 of
            EQ -> pure img
            LT -> pure $ Vty.cropTop remainingHeight img
            GT -> flip Vty.vertJoin img <$> go cs newRemainingHeight
