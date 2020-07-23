{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TupleSections              #-}
module Concur.Core.Types
  ( Widget(..)
  , mapView
  , wrapView
  , MonadUnsafeBlockingIO(..)
  , MultiAlternative(..)
  , liftSTM
  , view
  ) where

import           Control.Applicative      (Alternative, empty, (<|>))
import           Control.Concurrent       (forkIO)
import           Control.Concurrent.STM   (STM, newEmptyTMVarIO, atomically, putTMVar, takeTMVar, retry)
import           Control.Monad            (MonadPlus (..))
import           Control.Monad.IO.Class   (MonadIO, liftIO)
import           Control.MultiAlternative (MultiAlternative, orr, never)

import           Data.Bifunctor           (bimap)

newtype Widget v a = Widget { runWidget :: STM (Maybe v, Either a (IO (Widget v a))) }

instance Functor (Widget v) where
  fmap f (Widget w) = Widget $ do
    (v, r) <- w
    pure (v, bimap f (fmap (fmap f)) r)

instance Applicative (Widget v) where
  pure a = Widget $ pure (Nothing, Left a)
  f <*> a = do
    f' <- f
    a' <- a
    pure (f' a')

instance Monad (Widget v) where
  Widget w >>= f = Widget $ do
    (v, w') <- w
    pure $ (,) v $ case w' of
      Left a    -> Right (pure (f a))
      Right w'' -> Right $ do
        w''' <- w''
        pure (w''' >>= f)

view :: v -> Widget v a
view v = Widget (pure (Just v, Right $ pure forever))

effect :: IO a -> Widget v a
effect io = Widget $ pure $ (,) Nothing $ Right $ do
  v <- newEmptyTMVarIO
  _ <- forkIO (io >>= atomically . putTMVar v)

  pure $ Widget $ do
    a <- takeTMVar v
    pure (Nothing, Left a)

liftSTM :: STM a ->  Widget v a
liftSTM stm = Widget (stm >>= \a -> pure (Nothing, Left a))

forever :: Widget v a
forever = Widget retry

-- Change the view of a Widget
mapView :: (u -> v) -> Widget u a -> Widget v a
mapView f (Widget w) = Widget $ do
  (v, r) <- w
  pure (f <$> v, fmap (mapView f) <$> r)

-- Generic widget view wrapper
wrapView :: Applicative f => (u -> v) -> Widget u a -> Widget (f v) a
wrapView f = mapView (pure . f)

instance Monoid v => MonadIO (Widget v) where
  liftIO = effect

class MonadUnsafeBlockingIO m where
  liftUnsafeBlockingIO :: IO a -> m a

instance MonadUnsafeBlockingIO (Widget v) where
  liftUnsafeBlockingIO io = Widget $ pure $ (,) Nothing $ Right $ do
    a <- io
    pure $ Widget $ pure (Nothing, Left a)

instance Monoid v => Alternative (Widget v) where
  empty = never
  f <|> g = orr [f, g]

instance Monoid v => MultiAlternative (Widget v) where
  never = forever

  orr ws' = go (replicate (length ws') Nothing) ws'
    where
      go vs ws = Widget $ do
        (i, (v, w)) <- foldr (<|>) retry [ (i,) <$> runWidget w | (i, w) <- zip [0..] ws ]

        let vs' = take i vs <> [(vs !! i) <|> v] <> drop (i + 1) vs

        pure $ (,) (mconcat vs') $ case w of
          Left a   -> Left a
          Right w' -> Right $ do
            w'' <- w'
            pure $ orr (take i ws <> [w''] <> drop (i + 1) ws)

-- The default instance derives from Alternative
instance Monoid v => MonadPlus (Widget v)
