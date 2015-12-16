-- |
-- The types and functions are trivial and self-descriptive,
-- hence this sentence is the sole documentation you get on them.
module Success.Pure
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
import Data.Foldable
import Data.Traversable
import Control.Applicative
import Control.Monad
import Control.Monad.Error.Class


newtype Success a b =
  Success (Either (Maybe a) b)
  deriving (Functor, Applicative, Monad, MonadError (Maybe a), Show, Foldable, Traversable)

instance Alternative (Success a) where
  {-# INLINE empty #-}
  empty =
    Success (Left Nothing)
  {-# INLINE (<|>) #-}
  (<|>) =
    \case
      Success (Right x) -> const (Success (Right x))
      Success (Left _) -> id

instance MonadPlus (Success a) where
  {-# INLINE mzero #-}
  mzero =
    empty
  {-# INLINE mplus #-}
  mplus =
    (<|>)

{-# INLINE nothing #-}
nothing :: Success a b
nothing =
  Success (Left Nothing)

{-# INLINE failure #-}
failure :: a -> Success a b
failure failure =
  Success (Left (Just failure))

{-# INLINE success #-}
success :: b -> Success a b
success =
  pure

{-# INLINE asEither #-}
asEither :: Success a b -> Either (Maybe a) b
asEither (Success x) =
  x

{-# INLINE asMaybe #-}
asMaybe :: Success a b -> Maybe b
asMaybe (Success x) =
  case x of
    Left _ -> Nothing
    Right x -> Just x
