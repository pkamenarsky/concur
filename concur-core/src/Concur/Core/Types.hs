{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TupleSections              #-}
module Concur.Core.Types
  ( Widget(..)
  , display
  , mapView
  , wrapView
  , awaitViewAction
  , MultiAlternative(..)
  , loadWithIO
  , liftSTM
  , remoteWidget
  , unsafeBlockingIO
  , MonadUnsafeBlockingIO(..)
  , MonadSafeBlockingIO(..)
  ) where

import           Control.Applicative      (Alternative, empty, (<|>))
import           Control.Concurrent       (ThreadId, forkIO, killThread, newEmptyMVar, putMVar, takeMVar)
import           Control.Concurrent.STM   (STM, TMVar, newEmptyTMVarIO, atomically, putTMVar, takeTMVar, retry)
import           Control.Monad            (MonadPlus (..), forM)
import           Control.Monad.Free       (Free (..), hoistFree, liftF)
import           Control.Monad.IO.Class   (MonadIO, liftIO)
import           Control.MultiAlternative (MultiAlternative, orr, never)

import           Data.Bifunctor           (bimap)

import qualified Concur.Core.Notify       as N

newtype Widget v a = Widget { runWidget :: STM (Maybe v, Either a (Widget v a)) }

instance Functor (Widget v) where
  fmap f (Widget w) = Widget $ do
    (v, r) <- w
    pure (v, bimap f (fmap f) r)

instance Applicative (Widget v) where
  pure a = Widget $ pure (Nothing, Left a)
  f <*> a = do
    f' <- f
    a' <- a
    pure (f' a')

instance Monad (Widget v) where
  Widget w >>= f = Widget $ do
    (v, r) <- w
    pure $ (,) v $ case r of
      Left a   -> Right (f a)
      Right w' -> Right (w' >>= f)

view :: v -> Widget v a
view v = Widget (pure (Just v, Right $ Widget retry))

effect :: IO a -> Widget v a
effect a = undefined

io :: IO a ->  Widget v a
io a = undefined

liftSTM :: STM a ->  Widget v a
liftSTM stm = Widget (stm >>= \a -> pure (Nothing, Left a))

forever :: Widget v a
forever = Widget retry

display :: v -> Widget v a
display v = view v >> forever

-- Change the view of a Widget
mapView :: (u -> v) -> Widget u a -> Widget v a
mapView f (Widget w) = Widget $ do
  (v, r) <- w
  pure (f <$> v, mapView f <$> r)

-- Generic widget view wrapper
wrapView :: Applicative f => (u -> v) -> Widget u a -> Widget (f v) a
wrapView f = mapView (pure . f)

-- | IMPORTANT: Blocking IO is dangerous as it can block the entire UI from updating.
--   It should only be used for *very* quick running IO actions like creating MVars.
unsafeBlockingIO :: Monoid v => IO a -> Widget v a
unsafeBlockingIO = io

-- This is a safe use for blockingIO, and is exported
awaitViewAction :: (N.Notify a -> v) -> Widget v a
awaitViewAction f = do
  n <- io N.newNotify
  _ <- view (f n)
  effect $ N.await n

{-# DEPRECATED loadWithIO "Just use liftIO instead" #-}
loadWithIO :: IO a -> Widget v a
loadWithIO = effect

-- Make a Widget, which can be pushed to remotely
remoteWidget :: (MultiAlternative m, MonadUnsafeBlockingIO m, MonadSafeBlockingIO m, MonadIO m, Monad m, Show a) => m b -> (a -> m b) -> IO (a -> m (), m b)
remoteWidget d f = do
  var <- newEmptyMVar
  return (proxy var, wid var d)
  where
    proxy var = liftUnsafeBlockingIO . putMVar var
    wid var ui = orr [Left <$> ui, Right <$> liftSafeBlockingIO (takeMVar var)] >>= either return (wid var . f)

instance Monoid v => MonadIO (Widget v) where
  liftIO = loadWithIO

-- IMPORTANT NOTE: This Alternative instance is NOT the same one as that for Free.
-- That one simply uses Alternative for Suspend. But that one isn't sufficient for us.
-- Verify laws:
--         Right distributivity (of <*>):  (f <|> g) <*> a = (f <*> a) <|> (g <*> a)
--         Right absorption (for <*>):  empty <*> a = empty
--         Left distributivity (of fmap):  f <$> (a <|> b) = (f <$> a) <|> (f <$> b)
--  OK     Left absorption (for fmap):  f <$> empty = empty
instance Monoid v => Alternative (Widget v) where
  empty = never
  f <|> g = orr [f,g]

instance Monoid v => MultiAlternative (Widget v) where
  never = display mempty

  orr ws' = go (replicate (length ws') Nothing) ws'
    where
      go vs ws = Widget $ do
        (i, (v, w)) <- foldr (<|>) retry [ (i,) <$> runWidget w | (i, w) <- zip [0..] ws ]

        let vs' = take i vs <> [(vs !! i) <|> v] <> drop (i + 1) vs

        pure $ (,) (mconcat vs') $ case w of
          Left a -> Left a
          Right w' -> Right $ orr (take i ws <> [w'] <> drop (i + 1) ws)

-- The default instance derives from Alternative
instance Monoid v => MonadPlus (Widget v)

class MonadUnsafeBlockingIO m where
  liftUnsafeBlockingIO :: IO a -> m a

instance MonadUnsafeBlockingIO (Widget v) where
  liftUnsafeBlockingIO = io

class MonadSafeBlockingIO m where
  liftSafeBlockingIO :: IO a -> m a

instance MonadSafeBlockingIO (Widget v) where
  liftSafeBlockingIO = effect
