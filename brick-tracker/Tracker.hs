{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
module Tracker (tracker, AppState) where

import           Brick.AttrMap                 (attrMap)
import           Brick.BChan                   (BChan, newBChan, writeBChan)
import           Brick.Extra                   (renderBottomUp)
import           Brick.Focus                   (FocusRing)
import qualified Brick.Focus                   as FocusRing
import           Brick.Main                    (App (..), customMain, halt,
                                                showFirstCursor)
import           Brick.Types                   (BrickEvent (AppEvent, VtyEvent),
                                                EventM,
                                                Result (image), Size (Greedy),
                                                Widget (Widget))
import           Brick.Widgets.Border          (hBorder)
import           Brick.Widgets.Core            (fill, txt, (<=>))
import           Control.Concurrent            (ThreadId, forkIO)
import           Control.Concurrent.STM        (atomically)
import           Control.Concurrent.STM.TChan  (readTChan)
import           Control.Concurrent.STM.TMChan (TMChan)
import           Control.Monad                 (forever, join, void)
import           Control.Monad.Catch           (MonadMask)
import           Control.Monad.IO.Class        (MonadIO (..))
import           Data.Text                     (Text, pack)
import           Data.Text.IO                  (hGetLine)
import           Graphics.Vty                  as Vty (Event (EvKey),
                                                       Key (KChar),
                                                       Modifier (MCtrl), Vty,
                                                       defAttr, defaultConfig,
                                                       mkVty)
import           Lens.Micro                    ((^.))
import           Lens.Micro.Mtl                (use, (%=))
import           Lens.Micro.TH                 (makeLenses)
import           Sound.SuperCollider.Message   (AddAction (AddToTail), Message)
import           Sound.SuperCollider.Render    (RenderT, gated, play)
import           Sound.SuperCollider.Server    (HasSuperCollider (..),
                                                MonadServer (viewServer),
                                                Server, ServerT, handles,
                                                messages, newSynth, onServer,
                                                receiveSynthDef, startServer,
                                                withServer)
import           Sound.SuperCollider.SynthDef  (defaultSynthDef)
import           System.IO                     (Handle)
import           System.IO.Error               (catchIOError, isEOFError)

data AppEvent = SynthOutput Text
              | OSC Message
              deriving (Eq, Read, Show)

type WidgetName = ()

data AppState = State {
  _synthesizer :: Server
, _synthOutput :: [Text]
, _focusRing   :: FocusRing WidgetName
}

makeLenses ''AppState

appState :: Server -> AppState
appState s = State s [] (FocusRing.focusRing [])

instance HasSuperCollider AppState where
  supercollider = synthesizer

sound :: ServerT AppM a -> AppM a
sound x = (`onServer` x) =<< use supercollider

type AppM = EventM WidgetName AppState

buildVty :: IO Vty
buildVty = Vty.mkVty Vty.defaultConfig

welcome :: RenderT (ServerT AppM) ()
welcome = void $ gated 1 $ newSynth AddToTail "default" [("freq", 440)]

pattern Key :: String -> Vty.Event
pattern Key k <- (matchKey -> Just k)

matchKey :: Vty.Event -> Maybe String
matchKey = \case
  Vty.EvKey (Vty.KChar k) [Vty.MCtrl] -> Just $ 'C' : '-' : k : ""
  _ -> Nothing

tracker :: (MonadIO m, MonadMask m) => m AppState
tracker = withServer (startServer 57110) $ do
  (ha, hb, hc, hd) <- viewServer handles
  mq <- messages
  c <- liftIO $ newBChan 128
  void . liftIO . forkIO . forever $
    writeBChan c . OSC =<< atomically (readTChan mq)
  void $ readHandle SynthOutput c ha
  void $ readHandle SynthOutput c hb
  void $ readHandle SynthOutput c hc
  void $ readHandle SynthOutput c hd
  catchup <- receiveSynthDef defaultSynthDef
  vty <- liftIO buildVty
  catchup
  liftIO . customMain vty buildVty (Just c) app . appState =<< viewServer id
 where
  app = App { .. } where
    appStartEvent = sound $ play_ welcome
    appDraw s = [panel] where
      panel = fill ' ' <=> hBorder <=> synthLog
      synthLog = renderBottomUp $ txt <$> s ^. synthOutput
    appHandleEvent = \case
      AppEvent (SynthOutput x) -> synthOutput %= (x :)
      VtyEvent (Key "C-q") -> halt
      x -> synthOutput %= (pack (show x) :)
    appChooseCursor = showFirstCursor
    appAttrMap = const $ attrMap Vty.defAttr []

play_ :: RenderT (ServerT AppM) () -> ServerT AppM ()
play_ x = do
  (_ :: AppM (TMChan Rational), ()) <- play 0.05 x
  pure ()

readHandle :: MonadIO m => (Text -> e) -> BChan e -> Handle -> m ThreadId
readHandle c mq h = liftIO . forkIO . eof (const $ pure ()) . forever $
  writeBChan mq . c =<< hGetLine h

eof :: (IOError -> IO a) -> IO a -> IO a
eof x f = catchIOError f $ \e -> (if isEOFError e then x else ioError) e
