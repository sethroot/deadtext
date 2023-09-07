{-# LANGUAGE FlexibleContexts #-}

module Action.Use where

import Control.Error (hoistEither, runExceptT)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.State.Lazy (MonadState)
import Parser (recParseInvObj)
import Types

class Usable a where
    doUse :: (MonadState Game m) => a -> m String

instance Usable Obj where
    doUse (ObjInv _) = pure "using."
    doUse _          = pure ""

useAction :: (MonadState Game m, MonadIO m) => [Input] -> m ()
useAction inputs = do
    out <- use inputs
    either printE printE out
    where printE = liftIO . putStrLn

use :: MonadState Game m => [Input] -> m (Either String String)
use inputs = runExceptT $ do
    item'  <- recParseInvObj inputs
    result <- doUse item'
    hoistEither . Right $ result
