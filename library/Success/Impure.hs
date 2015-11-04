-- |
-- The types and functions are trivial and self-descriptive,
-- hence this sentence is the sole documentation you get on them.
module Success.Impure
(
  Success(..),
  run,
  nothing, failure, success,
)
where

import Prelude
import Control.Applicative
import Control.Monad
import qualified Success.Pure


newtype Success a m b =
  Success (m (Success.Pure.Success a b))
  deriving (Functor)

instance Applicative m => Applicative (Success e m) where
  {-# INLINE pure #-}
  pure a =
    Success (pure (Success.Pure.success a))
  {-# INLINE (<*>) #-}
  (<*>) (Success m1) (Success m2) =
    Success ((liftA2 . liftA2) ($) m1 m2)

instance Applicative m => Alternative (Success e m) where
  {-# INLINE empty #-}
  empty =
    Success (pure Success.Pure.nothing)
  {-# INLINE (<|>) #-}
  (<|>) (Success m1) (Success m2) =
    Success (liftA2 (<|>) m1 m2)

instance Monad m => Monad (Success e m) where
  {-# INLINE return #-}
  return =
    pure
  {-# INLINABLE (>>=) #-}
  (>>=) m1 m2' =
    Success (run m1 >>= m2 . Success.Pure.asEither)
    where
      m2 =
        \case
          Left Nothing -> pure Success.Pure.nothing
          Left (Just e) -> pure (Success.Pure.failure e)
          Right x -> run (m2' x)

instance Monad m => MonadPlus (Success e m) where
  {-# INLINE mzero #-}
  mzero =
    empty
  {-# INLINE mplus #-}
  mplus =
    (<|>)

{-# INLINE run #-}
run :: Success e m a -> m (Success.Pure.Success e a)
run (Success m) =
  m

{-# INLINE nothing #-}
nothing :: Applicative m => Success e m a
nothing =
  Success (pure Success.Pure.nothing)

{-# INLINE failure #-}
failure :: Applicative m => e -> Success e m a
failure details =
  Success (pure (Success.Pure.failure details))

{-# INLINE success #-}
success :: Applicative m => a -> Success e m a
success value =
  Success (pure (Success.Pure.success value))
