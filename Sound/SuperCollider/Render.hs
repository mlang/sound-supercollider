{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE ViewPatterns               #-}
module Sound.SuperCollider.Render (
  RenderT
, MonadRender(setTempo, plusTime)
, later, gated
, play
) where

import           Control.Concurrent                (forkIO)
import           Control.Concurrent.Async          (waitEither, withAsync)
import           Control.Concurrent.STM            (atomically)
import           Control.Concurrent.STM.TChan      (readTChan)
import           Control.Concurrent.STM.TMChan     (TMChan, closeTMChan,
                                                    newTMChanIO, writeTMChan)
import           Control.Monad.IO.Class            (MonadIO (..))
import           Control.Monad.Reader              (MonadReader (ask, local),
                                                    MonadTrans (..), void)
import           Control.Monad.Trans.Accum         (AccumT, accum, add, look,
                                                    looks, mapAccumT, runAccumT)
import qualified Control.Monad.Trans.State.Lazy    as Lazy
import qualified Control.Monad.Trans.State.Strict  as Strict
import qualified Control.Monad.Trans.Writer.CPS    as CPS
import qualified Control.Monad.Trans.Writer.Lazy   as Lazy
import qualified Control.Monad.Trans.Writer.Strict as Strict
import           Data.Foldable                     (Foldable (foldl', toList),
                                                    for_)
import           Data.Function                     (on)
import           Data.Group                        (Group (invert))
import           Data.List.NonEmpty                (NonEmpty (..), groupBy)
import qualified Data.List.NonEmpty                as NonEmpty (head)
import           Data.Map                          (Map)
import qualified Data.Map                          as Map
import           Data.Ord                          (comparing)
import           Data.Semigroup                    (Sum (..))
import           Data.Sort                         (sortBy)
import           Sound.Osc.Fd                      (Bundle (..), Time,
                                                    sendBundle, time)
import           Sound.SuperCollider.Message
import           Sound.SuperCollider.Server        ( MonadMessage (..),
                                                    MonadServer (..), messages,
                                                    setNodeControl, thread, udp)

newtype RenderT m a = RenderT (CPS.WriterT [(Rational, Message)]
                                       (AccumT (Pos Double Rational) m) a)
  deriving newtype ( Functor
                   , Applicative
                   , Monad
                   , MonadIO
                   )

instance MonadReader r m => MonadReader r (RenderT m) where
  ask = lift ask
  local f (RenderT m) = RenderT $ (CPS.mapWriterT . mapAccumT) (local f) m

instance Monad m => MonadMessage (RenderT m) where
  msg m = RenderT $ (lift . looks) (getSum . snd) >>= \t -> CPS.tell [(t, m)]

instance MonadTrans RenderT where
  lift = RenderT . lift . lift

instance (Monad m, MonadServer m) => MonadServer (RenderT m) where
  viewServer field = lift $ viewServer field
  shadow field value m = RenderT $ do
    p@(_, s) <- lift look
    ((a, w), (tm', s')) <- lift . lift . shadow field value $ runRenderT m p
    CPS.tell w
    lift . add $ (tm', s - s')
    pure a

runRenderT :: RenderT m a-> Pos Double Rational
           -> m ((a, [(Rational, Message)]), Pos Double Rational)
runRenderT (RenderT m) s = CPS.runWriterT m `runAccumT` s

class MonadRender m where
  setTempo :: Double -> m ()
  plusTime :: Rational -> m ()

instance Monad m => MonadRender (RenderT m) where
  setTempo t = RenderT . lift . accum $ \(snd -> Sum s) ->
    ((), (Map.singleton s t, mempty))
  plusTime a = RenderT . lift . add $ (mempty, Sum a)

instance (Monad m, MonadRender m) => MonadRender (Lazy.StateT s m) where
  setTempo = lift . setTempo
  plusTime = lift . plusTime

instance (Monad m, MonadRender m) => MonadRender (Strict.StateT s m) where
  setTempo = lift . setTempo
  plusTime = lift . plusTime

instance (Monad m, MonadRender m) => MonadRender (CPS.WriterT w m) where
  setTempo = lift . setTempo
  plusTime = lift . plusTime

instance (Monoid w, Monad m, MonadRender m) => MonadRender (Lazy.WriterT w m) where
  setTempo = lift . setTempo
  plusTime = lift . plusTime

instance (Monoid w, Monad m, MonadRender m) => MonadRender (Strict.WriterT w m) where
  setTempo = lift . setTempo
  plusTime = lift . plusTime

later :: Monad m => Rational -> RenderT m a -> RenderT m a
later (Sum -> t) (RenderT m) = RenderT $ do
  lift . add $ (mempty, t)
  a <- m
  lift . add $ (mempty, invert t)
  pure a

gated :: Monad m => Rational -> RenderT m NodeID -> RenderT m NodeID
gated d m = do
  nid <- m
  later d $ setNodeControl nid [("gate", 0)]
  pure nid

play :: (MonadServer m, MonadIO m, MonadIO n)
     => Time -> RenderT m a -> m (n (TMChan Rational), a)
play latency m = do
  ((a, groupBundles -> w), curry (getTime 1.0) . fst -> tm) <- runRenderT m mempty
  tc <- timeChannel $ synthTimeMap w
  u <- viewServer udp
  t0 <- (+ latency) <$> liftIO time
  for_ w $ \(t, xs) ->
    liftIO . sendBundle u . Bundle (t0 + tm (Sum t)) $ toList xs
  pure (tc, a)

timeChannel :: (Ord a, Num a, MonadServer m, MonadIO m, MonadIO n)
            => Map SynthID a -> m (n (TMChan a))
timeChannel m = do
  mq <- messages
  a <- viewServer thread
  pure . liftIO $ do
    c <- newTMChanIO
    if Map.null m
      then close c
      else void . forkIO . withAsync (go mq c (minimum (Map.elems m) - 1)) $ \x ->
             either (const $ close c) pure =<< waitEither a x
    pure c
 where
  endTime = maximum $ Map.elems m
  close = atomically . closeTMChan
  go mq c = loop where
    loop t = atomically (readTChan mq) >>= \case
      SynthCreated sid _ _ _ -> case Map.lookup sid m of
        Just t' | t' > t -> do
                    atomically $ writeTMChan c t'
                    if t' == endTime then close c else loop t'
        _ -> loop t
      _ -> loop t

synthTimeMap :: (Foldable f, Foldable g)
             => f (t, g Message) -> Map SynthID t
synthTimeMap = foldl' (\m -> uncurry $ \t -> foldl' (f t) m) Map.empty where
  f t m = \case
    NewSynth _ sid _ _ _ -> Map.insert sid t m
    _                    -> m

groupBundles :: Ord t => [(t, Message)] -> [(t, NonEmpty Message)]
groupBundles = map f . groupBy (on (==) fst) . sortBy (comparing fst) where
  f xs = (fst $ NonEmpty.head xs, snd <$> xs)

type Pos a b = (Map b a, Sum b)

getTime :: (Fractional a, Real b) => a -> Pos a b -> a
getTime z (m, Sum b) = sum . go . Map.toAscList $ Map.filterWithKey f m where
  f k = const $ k >= 0 && k < b
  go xs = zipWith g ((0, z):xs) (map fst xs++[b])
  g (a,s) c = realToFrac (c-a) * s
