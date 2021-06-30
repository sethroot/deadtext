{-# LANGUAGE FlexibleContexts #-}

module Common where

import           Control.Lens                   ( (^.)
                                                , use
                                                )
import           Control.Monad.State.Lazy       ( MonadState )
import qualified Data.Map                      as M
import           Types

npcIsHere :: MonadState Game m => Npc -> m Bool
npcIsHere npc = do
    loc' <- use loc
    pure $ npc ^. loc == loc'

itemIsHere :: MonadState Game m => Item -> m Bool
itemIsHere item = do
    loc' <- use loc
    pure $ item ^. loc == ItemLoc loc'

containerIsHere :: MonadState Game m => Container -> m Bool
containerIsHere cont = do
    loc' <- use loc
    pure $ cont ^. loc == loc'

inventory :: MonadState Game m => m [Item]
inventory = do
    items' <- use items
    pure $ filter (\x -> x ^. loc == ItemInv) items'
