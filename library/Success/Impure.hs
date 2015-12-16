{-# LANGUAGE UndecidableInstances #-}
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
import Control.Monad.IO.Class
import Control.Monad.Error.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Control
import Control.Monad.Base
import qualified Success.Pure


newtype Success a m b =
  Success (m (Success.Pure.Success a b))
  deriving (Functor, Foldable, Traversable)

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

instance (Applicative m, Monad m) => Monad (Success e m) where
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

instance (Applicative m, Monad m) => MonadPlus (Success e m) where
  {-# INLINE mzero #-}
  mzero =
    empty
  {-# INLINE mplus #-}
  mplus =
    (<|>)

instance (Applicative m, Monad m) => MonadError (Maybe a) (Success a m) where
  {-# INLINE throwError #-}
  throwError =
    Success . return . throwError
  {-# INLINE catchError #-}
  catchError (Success m) handler =
    Success $ m >>= either (unwrap . handler) (return . Success.Pure.success) . Success.Pure.asEither
    where
      unwrap (Success m) =
        m

instance MonadTrans (Success a) where
  {-# INLINE lift #-}
  lift =
    Success . liftM pure

instance (Applicative m, MonadIO m) => MonadIO (Success a m) where
  {-# INLINE liftIO #-}
  liftIO =
    lift . liftIO

instance (Applicative m, MonadBase n m) => MonadBase n (Success a m) where
  {-# INLINE liftBase #-}
  liftBase =
    lift . liftBase

instance MonadTransControl (Success a) where
  type StT (Success a) b =
    Success.Pure.Success a b
  {-# INLINE liftWith #-}
  liftWith onUnlift =
    lift $ onUnlift $ \(Success impl) -> impl
  {-# INLINE restoreT #-}
  restoreT =
    Success

instance (Applicative m, MonadBaseControl n m) => MonadBaseControl n (Success a m) where
  type StM (Success a m) b =
    ComposeSt (Success a) m b
  {-# INLINE liftBaseWith #-}
  liftBaseWith =
    defaultLiftBaseWith
  {-# INLINE restoreM #-}
  restoreM =
    defaultRestoreM

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
