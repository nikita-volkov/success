module Success
(
  Success,
  -- * Creation
  nothing,
  failure,
  success,
  -- * Execution
  asEither,
  asMaybe,
)
where

import Prelude
import Control.Applicative
import Control.Monad


newtype Success failure success =
  Success (Either (Maybe failure) success)
  deriving (Functor, Applicative, Monad)

instance Alternative (Success failure) where
  {-# INLINE empty #-}
  empty =
    Success (Left Nothing)
  {-# INLINE (<|>) #-}
  (<|>) =
    \case
      Success (Right x) -> const (Success (Right x))
      Success (Left _) -> id

instance MonadPlus (Success failure) where
  {-# INLINE mzero #-}
  mzero =
    empty
  {-# INLINE mplus #-}
  mplus =
    (<|>)

{-# INLINE nothing #-}
nothing :: Success failure success
nothing =
  Success (Left Nothing)

{-# INLINE failure #-}
failure :: failure -> Success failure success
failure failure =
  Success (Left (Just failure))

{-# INLINE success #-}
success :: success -> Success failure success
success =
  pure

{-# INLINE asEither #-}
asEither :: Success failure success -> Either (Maybe failure) success
asEither (Success x) =
  x

{-# INLINE asMaybe #-}
asMaybe :: Success failure success -> Maybe success
asMaybe (Success x) =
  case x of
    Left _ -> Nothing
    Right x -> Just x
