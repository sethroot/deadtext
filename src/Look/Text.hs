{-# LANGUAGE FlexibleContexts #-}
module Look.Text where

import Control.Monad.State.Lazy (MonadState)
import Types

enter :: MonadState Game m => MapLoc -> m String
enter mapLoc = case mapLoc of
    OverlookBathroom   -> undefined
    OverlookParkingLot -> undefined
    Cemetary           -> undefined
    Room202            -> room202Enter

room202Enter :: MonadState Game m => m String
room202Enter =
    pure
        . unwords
        $ [ "You are standing in room 202."
          , "In the middle of the room, you see a mannequin."
          , "A bright light shines at you."
          ]
