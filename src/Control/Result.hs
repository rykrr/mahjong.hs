module Control.Result where

import Control.Monad
import Control.Monad.Fail
import Data.Text

--------------------------------------------------------------------------------

data Result a = Ok a
              | Err Text
              deriving (Show, Eq)

--------------------------------------------------------------------------------

result :: (Text -> b) -> (a -> b) -> Result a -> b
result _ f (Ok  x) = f x
result f _ (Err s) = f s

fromResult :: a -> Result a -> a
fromResult x (Err _) = x
fromResult _ (Ok  x) = x

isOk :: Result a -> Bool
isOk (Ok _) = True
isOk _      = False

isErr :: Result a -> Bool
isErr r = not $ isOk r

resultFromMaybe :: Text -> Maybe a -> Result a
resultFromMaybe _         (Just x) = Ok x
resultFromMaybe errString Nothing  = Err errString

unwrap :: Result a -> a
unwrap (Ok x)  = x
unwrap (Err _) = undefined

--------------------------------------------------------------------------------

instance Functor Result where
    fmap f (Ok  x) = Ok (f x)
    fmap _ (Err e) = Err e

instance Applicative Result where
    Ok  f <*> x = fmap f x
    Err e <*> _ = Err e

    pure = Ok

instance Monad Result where
    Ok  x >>= f = f x
    Err e >>= _ = Err e

    Ok  x >> f = f
    Err e >> _ = Err e

    return = pure

instance MonadFail Result where
    fail = Err . pack

--------------------------------------------------------------------------------
