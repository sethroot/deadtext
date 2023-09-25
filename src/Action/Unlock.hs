{-# LANGUAGE FlexibleContexts #-}

module Action.Unlock (unlockAction) where
import Action.Use
import Control.Lens ((^.))
import Control.Monad.State.Lazy (MonadState)
import Data.Monoid (All(All, getAll))
import Types

unlockAction :: MonadState Game m => [Input] -> m (Either String String)
unlockAction [] = pure . Right $ "Unlock what?"
unlockAction inputs
    | matchesText "door with key" inputs = Action.Use.useAction [Input "" "key"]
    | otherwise                          = pure . Right $ "what"

matchesText :: String -> [Input] -> Bool
matchesText text inputs =
    let
        words' = words text
        sameLength = length words' == length inputs
        alls   = zipWith (\t i -> All $ i ^. normal == t) words' inputs
        inputMatchesText = getAll . mconcat $ alls
    in sameLength && inputMatchesText 
