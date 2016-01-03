{-# LANGUAGE Rank2Types #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.Free.VanLaarhoven
-- Copyright   :  (C) 2016 Aaron Levin
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Aaron Levin <aaron.michael.benjamin.levin@gmail.com>
-- Stability   :  provisional
-- Portability :  non-portable (rank-2 polymorphism)
--
-- \"van Laarhoven encoded Free Monad\"
-----------------------------------------------------------------------------

module Control.Monad.Free.VanLaarhoven
  ( Free(..)
  ) where

import Control.Arrow ((&&&))

-- | The van Laarhoven-encoded Free Monad
newtype Free effect a =
  Free { runFree :: forall m. Monad m => effect m -> m a }

instance Functor (Free effect) where
  fmap f (Free run) = Free (fmap f . run)

instance Applicative (Free effect) where
  pure a = Free (const (pure a))
  (Free fab) <*> (Free a) =
    Free (\e -> fab e <*> a e)

instance Monad (Free effect) where
  (Free run) >>= f =
    Free (\e -> run e >>= \a -> runFree (f a) e)
