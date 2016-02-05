{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE TypeOperators         #-}
#if !MIN_VERSION_base(4,8,0)
{-# LANGUAGE OverlappingInstances  #-}
#endif

-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.Free.VanLaarhovenE
-- Copyright   :  (C) 2016 Aaron Levin
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Aaron Levin <aaron.michael.benjamin.levin@gmail.com>
-- Stability   :  provisional
-- Portability :  non-portable (rank-2 polymorphism)
--
-- \"van Laarhoven encoded Free Monad with extensible effects\"
-----------------------------------------------------------------------------

module Control.Monad.Free.VanLaarhovenE
  ( (.:.)
  , Effects (..)
  , Free(..)
  , HasEffect(..)
  , iterM
  , liftF
  ) where

import           Control.Applicative
import           Control.Arrow       ((&&&))

-- | a customized HList of effects. We need to carry the 'm' param around for
-- type inference.
data Effects a (m :: * -> *) where
  EmptyE :: Effects '[] m
  ConsE :: effect m -> Effects effects m -> Effects (effect ': effects) m

-- | Helper combinator for creating values of 'Effects effects m'
(.:.) :: effect m -> Effects effects m -> Effects (effect ': effects) m
effect .:. effects = ConsE effect effects
infixr 4 .:.

-- | The van Laarhoven-encoded Free Monad with Extensible effects
newtype Free effects a =
  Free { runFree :: forall m. Monad m => Effects effects m -> m a }

instance Functor (Free effect) where
  fmap f (Free run) = Free (fmap f . run)

instance Applicative (Free effect) where
  pure a = Free (const (pure a))
  (Free fab) <*> (Free a) =
    Free (\e -> fab e <*> a e)

instance Monad (Free effect) where
  (Free run) >>= f =
    Free (\e -> run e >>= \a -> runFree (f a) e)

-- | A class to help us fetch effects from our effect stack.
class HasEffect (effects :: [((* -> *) -> *)]) (effect :: ((* -> *) -> *)) where
  getEffect :: Effects effects m -> effect m

-- | An instance of 'HasEffect'  that handles the case where our desired effect
-- type doesn't match the head of the HList.
instance
#if MIN_VERSION_base(4,8,0)
         {-# OVERLAPPABLE #-}
#endif
  HasEffect effects effect => HasEffect (notIt ': effects) effect where
  getEffect (ConsE _ effects) = getEffect effects

-- | An instance of 'HasEffect' that handles the case where our desired effect
-- type matches the head of the list. We then return that effect.
instance
#if MIN_VERSION_base(4,8,0)
         {-# OVERLAPPABLE #-}
#endif
  HasEffect (effect ': effects) effect where
  getEffect (ConsE effect _) = effect

-- | A version of lift that can be used with an effect stack.
liftF :: HasEffect effects effect    -- constraint that ensures our effect is in the effect stack
      => (forall m. effect m -> m a) -- method to pull our operation from our effect
      -> Free effects a
liftF getOp = Free (getOp . getEffect)

-- | Tear down a 'Free' 'Monad' using the supplied effects value.
iterM :: Monad m
      => Effects effects m
      -> Free effects a
      -> m a
iterM phi program = runFree program phi
