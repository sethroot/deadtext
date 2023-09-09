{-# LANGUAGE FlexibleContexts #-}

module Action.Use where

import Control.Error ((!?), hoistEither, runExceptT)
import Control.Monad.State.Lazy (MonadState)
import Parser (recParseInvObj)
import Types
import Util (hoistR)

class Usable a where
    doUse :: (MonadState Game m) => a -> m String

instance Usable Obj where
    doUse (ObjInv _) = pure "using."
    doUse _          = undefined

useAction :: MonadState Game m => [Input] -> m (Either String String)
useAction inputs = runExceptT $ do
    item'  <- recParseInvObj inputs !? doNotHaveItem
    result <- doUse item'
    hoistR result

doNotHaveItem :: String
doNotHaveItem = "You do not have that item."
