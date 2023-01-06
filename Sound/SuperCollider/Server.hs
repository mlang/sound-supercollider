{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE Strict                     #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE ViewPatterns               #-}
module Sound.SuperCollider.Server (
  Server, ServerT, handles
, startServer, stopServer
, onServer, withServer
, HasSuperCollider(supercollider)
, MonadServer(viewServer, shadow), thread, udp
, MonadMessage(msg)
, messages
, status, version, send, sync
, receiveSynthDef, setNodeControl
, waitSilence
, newGroup, newSynth, inGroup
) where
import           Brick.Types                  (EventM)
import           Control.Concurrent           (MVar, ThreadId, forkIO,
                                               newEmptyMVar, newMVar, putMVar,
                                               takeMVar, threadDelay)
import           Control.Concurrent.Async
import           Control.Concurrent.STM       (atomically)
import           Control.Concurrent.STM.TChan
import           Control.Monad.Catch          (MonadCatch, MonadMask,
                                               MonadThrow, bracket)
import           Control.Monad.Reader
import           Control.Monad.Trans.Accum
import qualified Control.Monad.Trans.RWS.CPS as CPS.RWS
import qualified Control.Monad.Trans.RWS.Lazy as Lazy.RWS
import qualified Control.Monad.Trans.RWS.Strict as Strict.RWS
import qualified Control.Monad.Trans.State.Lazy as Lazy
import qualified Control.Monad.Trans.State.Strict as Strict
import qualified Control.Monad.Trans.Writer.CPS as CPS
import qualified Control.Monad.Trans.Writer.Lazy as Lazy
import qualified Control.Monad.Trans.Writer.Strict as Strict
import           Data.Bits                    (shiftL)
import           Data.Foldable                (for_)
import           Data.Function                ((&))
import           Data.Functor                 ((<&>))
import           Data.Int                     (Int32)
import qualified Data.Set                     as Set (delete, empty, insert,
                                                      null)
import           Lens.Micro                   (Lens', (.~))
import           Lens.Micro.Contra
import           Lens.Micro.Mtl               (use, view, (.=))
import           Sound.Osc.Fd                 (Ascii, Transport, Udp, openUdp,
                                               recvMessages, sendMessage,
                                               withTransport)
import           Sound.SuperCollider.Message
import           Sound.SuperCollider.SynthDef
import           System.IO                    (Handle)
import           System.Process.Typed         (checkExitCode, createPipe,
                                               getStderr, getStdout, nullStream,
                                               proc, setEnv, setStderr,
                                               setStdin, setStdout,
                                               withProcessWait)
import           System.Timeout               (timeout)

data Server = Server (Async ()) Udp (TChan Message) ClientID (MVar Allocator)
                     GroupID (Handle, Handle, Handle, Handle) (Server -> IO ())

thread :: Getter Server (Async ())
thread h s@(Server a _ _ _ _ _ _ _) = (const s) <$> h a

udp :: Getter Server Udp
udp h s@(Server _ a _ _ _ _ _ _) = (const s) <$> h a

channel :: Getter Server (TChan Message)
channel h s@(Server _ _ a _ _ _ _ _) = (const s) <$> h a

allocator :: Getter Server (MVar Allocator)
allocator h s@(Server _ _ _ _ a _ _ _) = (const s) <$> h a

defaultGroup :: Lens' Server GroupID
defaultGroup x (Server a b c d e f g h) = x f <&> \f' -> Server a b c d e f' g h

handles :: Getter Server (Handle, Handle, Handle, Handle)
handles x s@(Server _ _ _ _ _ _ a _) = const s <$> x a

stop :: Getter Server (Server -> IO ())
stop h s@(Server _ _ _ _ _ _ _ a) = (const s) <$> h a

class HasSuperCollider a where
  supercollider :: Lens' a Server

instance HasSuperCollider Server where
  supercollider = id

newtype ServerT m a = ServerT (ReaderT Server m a)
  deriving newtype ( Functor
                   , Applicative
                   , Monad
                   , MonadCatch
                   , MonadIO
                   , MonadMask
                   , MonadThrow
                   )

class MonadServer m where
  viewServer :: Getter Server a -> m a
  shadow :: Lens' Server a -> a -> m b -> m b

instance Monad m => MonadServer (ServerT m) where
  viewServer field = ServerT $ view field
  shadow field value (ServerT m) = ServerT $ local (field .~ value) m

instance HasSuperCollider s => MonadServer (EventM n s) where
  viewServer field = use $ supercollider . field
  shadow field value m = bracket remember restore go where
    remember = use $ supercollider . field
    restore v = supercollider . field .= v
    go _ = do
      supercollider . field .= value
      m

instance (Monoid w, Monad m, MonadServer m) => MonadServer (AccumT w m) where
  viewServer field = lift $ viewServer field
  shadow field value m = do
    (a, w) <- lift . shadow field value . runAccumT m =<< look
    add w
    pure a

instance (Monad m, MonadServer m) => MonadServer (ReaderT r m) where
  viewServer field = lift $ viewServer field
  shadow field value m = lift . shadow field value . runReaderT m =<< ask

instance (Monoid w, Monad m, MonadServer m) => MonadServer (CPS.RWS.RWST r w s m) where
  viewServer field = lift $ viewServer field
  shadow field value m = do
    r <- CPS.RWS.ask
    (a, s, w) <- lift . shadow field value . CPS.RWS.runRWST m r =<< CPS.RWS.get
    CPS.RWS.put s
    CPS.RWS.tell w
    pure a

instance (Monoid w, Monad m, MonadServer m) => MonadServer (Lazy.RWS.RWST r w s m) where
  viewServer field = lift $ viewServer field
  shadow field value m = do
    r <- Lazy.RWS.ask
    (a, s, w) <- lift . shadow field value . Lazy.RWS.runRWST m r =<< Lazy.RWS.get
    Lazy.RWS.put s
    Lazy.RWS.tell w
    pure a

instance (Monoid w, Monad m, MonadServer m) => MonadServer (Strict.RWS.RWST r w s m) where
  viewServer field = lift $ viewServer field
  shadow field value m = do
    r <- Strict.RWS.ask
    (a, s, w) <- lift . shadow field value . Strict.RWS.runRWST m r =<< Strict.RWS.get
    Strict.RWS.put s
    Strict.RWS.tell w
    pure a

instance (Monad m, MonadServer m) => MonadServer (Lazy.StateT s m) where
  viewServer field = lift $ viewServer field
  shadow field value m = do
    (a, s) <- lift . shadow field value . Lazy.runStateT m =<< Lazy.get
    Lazy.put s
    pure a

instance (Monad m, MonadServer m) => MonadServer (Strict.StateT s m) where
  viewServer field = lift $ viewServer field
  shadow field value m = do
    (a, s) <- lift . shadow field value . Strict.runStateT m =<< Strict.get
    Strict.put s
    pure a

instance (Monoid w, Monad m, MonadServer m) => MonadServer (CPS.WriterT w m) where
  viewServer field = lift $ viewServer field
  shadow field value m = do
    (a, w) <- lift . shadow field value . CPS.runWriterT $ m
    CPS.tell w
    pure a

instance (Monoid w, Monad m, MonadServer m) => MonadServer (Lazy.WriterT w m) where
  viewServer field = lift $ viewServer field
  shadow field value m = do
    (a, w) <- lift . shadow field value . Lazy.runWriterT $ m
    Lazy.tell w
    pure a

instance (Monoid w, Monad m, MonadServer m) => MonadServer (Strict.WriterT w m) where
  viewServer field = lift $ viewServer field
  shadow field value m = do
    (a, w) <- lift . shadow field value . Strict.runWriterT $ m
    Strict.tell w
    pure a

startServer :: MonadIO m => Int -> m Server
startServer port = liftIO $ do
  ok <- newEmptyMVar
  a <- async $ do
    mq <- newBroadcastTChanIO
    -- debug <- atomically $ dupTChan mq
    -- forkIO $ forever $ atomically (readTChan debug) >>= print
    withProcessWait (jackd 128 "hw:sofhdadsp") $ \j -> do
      threadDelay 300000
      withProcessWait (scsynth port 2) $ \sc -> do
        threadDelay 300000
        withTransport (openUdp "127.0.0.1" port) $ \t -> do
          putMVar ok (t, mq, (getStdout j, getStderr j, getStdout sc, getStderr sc))
          readMsgs t mq
        checkExitCode sc
      checkExitCode j
  (t, mq, hdls) <- checkAsync a $ takeMVar ok
  c <- atomically . dupTChan $ mq
  sendMessage t $ Notify True
  (cid, logins) <- checkAsync a $ notifyReply c
  let (dgid, alloc) = allocatePermanentNodeID $ mkAllocator cid logins
  sendMessage t $ NewGroup dgid AddToTail 0
  Server a t mq cid <$> newMVar alloc
                    <*> pure dgid
                    <*> pure hdls
                    <*> pure (`onServer` quit)

quit :: ServerT IO ()
quit = do
  a <- viewServer thread
  send Quit
  liftIO $ maybe (uninterruptibleCancel a) pure =<< timeout 1000000 (wait a)

stopServer :: MonadIO m => Server -> m ()
stopServer s@(view stop -> f) = liftIO $ f s

withServer :: (MonadMask m, MonadIO m) => m Server -> ServerT m a -> m a
withServer open (ServerT f) = bracket open stopServer $ runReaderT f

receiveSynthDef :: ( MonadServer m, MonadIO m, MonadIO n )
                => SynthDef -> m (n ())
receiveSynthDef d = do
  mq <- messages
  a <- viewServer thread
  send $ ReceiveSynthDef d
  pure . checkAsync a $ check mq
 where
  check mq = loop where
    loop = atomically (readTChan mq) >>= \case
      ReceiveSynthDefDone -> pure ()
      _                   -> loop

newSynth :: ( MonadServer m, MonadMessage m, MonadIO m )
         => AddAction -> Ascii -> [(ControlIndex, ControlValue)] -> m SynthID
newSynth aa n xs = do
  sid <- allocTempNodeID
  msg =<< NewSynth n sid aa <$> viewServer defaultGroup <*> pure xs
  pure sid

class MonadMessage m where
  msg :: Message -> m ()

instance (MonadIO m) => MonadMessage (ServerT m) where
  msg = send

instance HasSuperCollider s => MonadMessage (EventM n s) where
  msg = send

instance (Monad m, MonadMessage m) => MonadMessage (ReaderT r m) where
  msg = lift . msg

instance (Monad m, MonadMessage m) => MonadMessage (Lazy.StateT s m) where
  msg = lift . msg

instance (Monad m, MonadMessage m) => MonadMessage (Strict.StateT s m) where
  msg = lift . msg

instance (Monad m, MonadMessage m) => MonadMessage (CPS.WriterT w m) where
  msg = lift . msg

instance (Monoid w, Monad m, MonadMessage m) => MonadMessage (Lazy.WriterT w m) where
  msg = lift . msg

instance (Monoid w, Monad m, MonadMessage m) => MonadMessage (Strict.WriterT w m) where
  msg = lift . msg

allocTempNodeID :: (MonadIO m, MonadServer m) => m NodeID
allocTempNodeID = do
  ma <- viewServer allocator
  liftIO $ do
    (nid, a) <- allocateTemporaryNodeID <$> takeMVar ma
    putMVar ma $! a
    pure nid

newGroup :: (MonadServer m, MonadMessage m, MonadIO m)
         => AddAction -> m NodeID
newGroup aa = do
  gid <- allocTempNodeID
  msg . NewGroup gid aa =<< viewServer defaultGroup
  pure gid

setNodeControl :: MonadMessage m
               => NodeID -> [(ControlIndex, ControlValue)] -> m ()
setNodeControl nid = msg . SetNodeControl nid

onServer :: Server -> ServerT m a -> m a
onServer s (ServerT m) = runReaderT m s

inGroup :: MonadServer m => GroupID -> m a -> m a
inGroup gid = shadow defaultGroup gid

cmd :: (MonadServer m, MonadIO m)
    => Message -> (Message -> Maybe a) -> m a
cmd m f = do
  mq <- messages
  a <- viewServer thread
  send m
  checkAsync a $ check mq
 where
  check mq = loop where
    loop = maybe loop pure =<< f <$> atomically (readTChan mq)

version :: (MonadServer m, MonadIO m)
        => m (Ascii, Int32, Int32, Ascii, Ascii, Ascii)
version = cmd Version $ \case
  VersionReply name major minor patch vcbranch vchash ->
    Just (name, major, minor, patch, vcbranch, vchash)
  _ -> Nothing

status :: (MonadServer m, MonadIO m)
       => m (Int32, Int32, Int32, Int32, Float, Float, Double, Double)
status = cmd Status $ \case
  StatusReply ugens synths groups synthDefs al pl nsr asr ->
    Just (ugens, synths, groups, synthDefs, al, pl, nsr, asr)
  _ -> Nothing

-- allocRead :: (MonadServer m1, MonadIO m1, MonadIO m2) => BufferID -> Filename -> Sound.SuperCollider.Message.StartFrame -> Sound.SuperCollider.Message.FrameCount -> m1 (m2 ())
allocRead id fp st c = do
  mq <- messages
  a <- viewServer thread
  send $ AllocReadBuffer id fp st c
  pure . checkAsync a $ check mq
 where
  check mq = loop where
    loop = atomically (readTChan mq) >>= \case
      AllocReadBufferDone id' | id == id' -> pure ()
      _                                   -> loop

scsynth udp out =
  proc "scsynth" [
    "-u", show udp, "-o", show out, "-R", show 0, "-L", "-D", show 0
  ] & setEnv [("SC_JACK_DEFAULT_OUTPUTS", "system")]
    & setStdin nullStream
    & setStdout createPipe
    & setStderr createPipe

jackd buf dev =
  proc "jackd" [
    "-T", "-d", "alsa", "-P", "-p", show buf, "-d", dev
  ] & setEnv [("JACK_NO_AUDIO_RESERVATION", "1")]
    & setStdin nullStream
    & setStdout createPipe
    & setStderr createPipe

allocatePermanentNodeID :: Allocator -> (NodeID, Allocator)
allocatePermanentNodeID a = (permNID a, a { permNID = permNID a + 1 })

allocateTemporaryNodeID :: Allocator -> (NodeID, Allocator)
allocateTemporaryNodeID a = (tempNID a, a { tempNID = tempNID a + 1 })

mkAllocator :: ClientID -> Int32 -> Allocator
mkAllocator (ClientID cid) maxLogins = Allocator (r + 1) (r + 1000) where
  r = cid `shiftL` 26

data Allocator = Allocator {
  permNID :: NodeID
, tempNID :: NodeID
} deriving (Eq, Read, Show)

checkAsync :: MonadIO m => Async a -> IO b -> m b
checkAsync a m = liftIO $ withAsync m $ \x -> waitEither a x >>=
  either (const $ error "processes unexpectedly exited") pure

notifyReply :: TChan Message -> IO (ClientID, Int32)
notifyReply mq = loop where
  loop = atomically (readTChan mq) >>= \case
    NotifyReply cid logins -> pure (cid, logins)
    _                      -> loop

send :: (MonadServer m, MonadIO m) => Message -> m ()
send m = do
  a <- viewServer thread
  u <- viewServer udp
  checkAsync a $ sendMessage u m

messages :: (MonadServer m, MonadIO m) => m (TChan Message)
messages = liftIO . atomically . dupTChan =<< viewServer channel

sync :: (MonadServer m, MonadIO m) => m ()
sync = do
  mq <- messages
  send (Sync 1)
  liftIO . check $ mq
 where
  check mq = loop where
    loop = atomically (readTChan mq) >>= \case
      SyncReply 1 -> pure ()
      _           -> loop

waitSilence :: (MonadServer m, MonadIO m, MonadIO n) => m (n ())
waitSilence = do
  mq <- messages
  a <- viewServer thread
  pure . checkAsync a $ check mq
 where
  check mq = loop Set.empty False where
    loop l x
      | x && Set.null l = pure ()
      | otherwise = atomically (readTChan mq) >>= \case
          SynthCreated sid _ _ _ -> loop (Set.insert sid l) True
          SynthEnded sid _ _ _   -> loop (Set.delete sid l) x
          _                            -> loop l x

readMsgs :: Transport t => t -> TChan Message -> IO ()
readMsgs t mq = loop where
  loop = do
    ms <- recvMessages t
    for_ ms $ \case
      QuitDone -> pure ()
      m        -> atomically (writeTChan mq m) *> loop
