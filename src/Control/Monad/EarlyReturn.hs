module Control.Monad.EarlyReturn where

import Universum

import Control.Monad.Trans.Except (throwE)

type EarlyReturnT r = ExceptT r

withEarlyReturn :: Functor m => EarlyReturnT a m a -> m a
withEarlyReturn = map (either id id) . runExceptT

earlyReturn :: Monad m => a -> EarlyReturnT a m b
earlyReturn = throwE
