{-# LANGUAGE FlexibleContexts #-}

module Common where

import           Control.Lens                   ( (^.)
                                                , use
                                                )
import           Control.Monad.State.Lazy       ( MonadState )
import           Data.Char                      ( toLower )
import           Types
import           Util                           ( (?) )

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

inInventory :: HasLoc s ItemLocation => s -> Bool
inInventory item = item ^. loc == ItemInv

inventory :: MonadState Game m => m [Item]
inventory = do
    items' <- use items
    pure $ filter inInventory items'

indefArt :: String -> String
indefArt s = isVowel ? "an" $ "a"
    where isVowel = toLower (head s) `elem` ['a', 'e', 'i', 'o', 'u']
