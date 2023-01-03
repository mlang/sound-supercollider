{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}
module Sound.SuperCollider.Render.MIDI (midiFile) where

import           Control.Monad.IO.Class           (MonadIO (liftIO))
import           Control.Monad.State              (evalStateT, gets, modify')
import qualified Data.EventList.Relative.TimeBody as EventList
import qualified Data.Map                         as Map
import           Data.Ratio                       ((%))
import qualified Sound.MIDI.File                  as MIDIFile
import qualified Sound.MIDI.File.Event            as Event
import qualified Sound.MIDI.File.Event.Meta       as MetaEvent
import qualified Sound.MIDI.Message.Channel       as ChannelMsg
import qualified Sound.MIDI.Message.Channel.Voice as VoiceMsg
import           Sound.SuperCollider.Message
import           Sound.SuperCollider.Render       (RenderT, plusTime, setTempo)
import           Sound.SuperCollider.Server       (MonadServer (..), newSynth,
                                                   setNodeControl)

midiFile :: (MonadServer m, MonadIO m)
         => MIDIFile.T -> RenderT m ()
midiFile = match . MIDIFile.explicitNoteOff where
  match (MIDIFile.Cons ty (MIDIFile.ticksPerQuarterNote -> ppq) ts) =
    go (% fromIntegral ppq) $ MIDIFile.mergeTracks ty ts
  go r xs = EventList.mapM_ (time r) event xs `evalStateT` Map.empty
  time rational = plusTime . rational . fromIntegral
  event = \case
    Event.MetaEvent (MetaEvent.SetTempo micros) -> do
      setTempo (realToFrac micros / 1000000)
    Event.MIDIEvent (ChannelMsg.Cons c (ChannelMsg.Voice vm)) -> case vm of
      VoiceMsg.NoteOn p v -> do
        let freq = VoiceMsg.frequencyFromPitch p
        sid <- newSynth AddToTail "default" [("freq", freq)]
        modify' $ Map.insert (c, p) sid
      VoiceMsg.NoteOff p _ -> do
        gets (Map.lookup (c, p)) >>= \case
          Just sid -> do
            setNodeControl sid [("gate", 0)]
            modify' $ Map.delete (c, p)
          Nothing -> pure ()
      _ -> pure ()
    _ -> pure ()
