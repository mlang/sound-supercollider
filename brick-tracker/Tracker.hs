{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
module Tracker (tracker, AppState) where

import           Brick.AttrMap                 (attrMap)
import           Brick.BChan                   (BChan, newBChan, writeBChan)
import           Brick.Extra                   (renderBottomUp)
import           Brick.Focus                   (FocusRing, focusGetCurrent,
                                                focusNext)
import qualified Brick.Focus                   as FocusRing
import           Brick.Main                    (App (..), customMain, halt,
                                                showFirstCursor)
import           Brick.Types                   (BrickEvent (AppEvent, VtyEvent),
                                                EventM, Widget)
import           Brick.Widgets.Border          (hBorder)
import           Brick.Widgets.Core            (str, txt, (<=>), joinBorders)
import           Control.Applicative           (Applicative (liftA2))
import           Control.Concurrent            (ThreadId, forkIO)
import           Control.Concurrent.STM        (atomically)
import           Control.Concurrent.STM.TChan  (readTChan)
import           Control.Concurrent.STM.TMChan (TMChan)
import           Control.Monad                 (forever, void)
import           Control.Monad.Catch           (MonadMask)
import           Control.Monad.IO.Class        (MonadIO (..))
import           Control.Monad.Reader          (Reader, runReader)
import           Data.Text                     (Text)
import           Data.Text.IO                  (hGetLine)
import           Graphics.Vty                  as Vty (Vty, defAttr,
                                                       defaultConfig, mkVty)
import           Graphics.Vty.Patterns         (pattern Key)
import           Lens.Micro.Mtl                (use, view, zoom, (%=))
import           Lens.Micro.TH                 (makeLenses)
import           Pianoroll
import           Sound.SuperCollider.Message   (AddAction (AddToTail), Message)
import           Sound.SuperCollider.Render    (RenderT, gated, play)
import           Sound.SuperCollider.Server    (HasSuperCollider (..),
                                                MonadServer (viewServer),
                                                Server, ServerT, handles,
                                                messages, newSynth, onServer,
                                                receiveSynthDef, startServer,
                                                withServer)
import           Sound.SuperCollider.SynthDef.Builtin  (def)
import           System.IO                     (Handle)
import           System.IO.Error               (catchIOError, isEOFError)

data AppEvent = SynthOutput Text
              | OSC Message
              deriving (Eq, Read, Show)

data WidgetName = PianorollLabels | Pianoroll deriving (Eq, Ord, Show, Read)

data AppState e n = State {
  _synthesizer  :: !Server
, _brickChannel :: !(BChan e)
, _synthOutput  :: [Widget n]
, _focusRing    :: !(FocusRing n)
, _pianoroll    :: Pianoroll
}

makeLenses ''AppState

appState :: BChan e -> Server -> AppState e WidgetName
appState c s = State s c [] (FocusRing.focusRing [Pianoroll]) defaultPianoroll

instance HasSuperCollider (AppState e n) where supercollider = synthesizer

sound :: ServerT AppM a -> AppM a
sound x = (`onServer` x) =<< use supercollider

type AppM = EventM WidgetName (AppState AppEvent WidgetName)
type Draw = Reader (AppState AppEvent WidgetName)

isFocused :: WidgetName -> Draw Bool
isFocused n = (== Just n) . focusGetCurrent <$> view focusRing

draw :: Draw [Widget WidgetName]
draw = do
  roll <- liftA2 (renderPianoroll PianorollLabels Pianoroll) (view pianoroll) (isFocused Pianoroll)
  sLog <- renderBottomUp <$> view synthOutput
  let panel = joinBorders $ roll <=> hBorder <=> sLog
  pure $ [panel]

buildVty :: IO Vty
buildVty = Vty.mkVty Vty.defaultConfig

welcome :: RenderT (ServerT AppM) ()
welcome = void $ gated 1 $ newSynth AddToTail "default" [("freq", 440)]

tracker :: (MonadIO m, MonadMask m) => Int -> m (AppState AppEvent WidgetName)
tracker port = withServer (startServer port) $ do
  (ha, hb, hc, hd) <- viewServer handles
  mq <- messages
  c <- liftIO $ newBChan 128
  void . liftIO . forkIO . forever $
    writeBChan c . OSC =<< atomically (readTChan mq)
  void $ readHandle SynthOutput c ha
  void $ readHandle SynthOutput c hb
  void $ readHandle SynthOutput c hc
  void $ readHandle SynthOutput c hd
  catchup <- receiveSynthDef def
  vty <- liftIO buildVty
  catchup
  liftIO . customMain vty buildVty (Just c) app . appState c =<< viewServer id
 where
  app = App { .. } where
    appStartEvent = sound $ play_ welcome
    appDraw = runReader draw
    appHandleEvent = \case
      AppEvent (SynthOutput x) -> synthOutput %= (txt x :)
      VtyEvent (Key "TAB")   -> focusRing %= focusNext
      VtyEvent (Key "C-M-q") -> halt
      VtyEvent (Key "RET")   -> do
        freq <- getPitch <$> use pianoroll
        sound . play_ . void . gated 1 $
          newSynth AddToTail "default" [("freq", freq)]
      e                      -> focusGetCurrent <$> use focusRing >>= \case
        Just Pianoroll -> zoom pianoroll $ handlePianorollEvent e
        Nothing        -> synthOutput %= (str "No focus" :)
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
