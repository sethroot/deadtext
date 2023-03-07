{-# LANGUAGE FlexibleContexts #-}

module Common where

import Control.Lens ((^.), use)
import Control.Monad.State.Lazy (MonadState)
import Data.Char (toLower)
import Types
import Util ((?))

npcIsHere :: MonadState Game m => Npc -> m Bool
npcIsHere npc = do
    loc' <- use loc
    pure $ npc ^. loc == loc'

itemIsHere :: MonadState Game m => Item -> m Bool
itemIsHere i = do
    loc' <- use loc
    pure $ i ^. loc == ItemLoc loc'

containerIsHere :: MonadState Game m => Container -> m Bool
containerIsHere cont = do
    loc' <- use loc
    pure $ cont ^. loc == loc'

inInventory :: HasLoc s ItemLocation => s -> Bool
inInventory a = a ^. loc == ItemInv

inventory :: MonadState Game m => m [Item]
inventory = do
    items' <- use items
    pure $ filter inInventory items'

indefArt :: String -> String
indefArt s = isVowel ? "an" $ "a"
    where isVowel = toLower (head s) `elem` ['a', 'e', 'i', 'o', 'u']

period :: String -> String
period s = s ++ "."

outF :: String
outF = "Something has gone horribly wrong."

dontKnowHowToDoThat :: String
dontKnowHowToDoThat = "Don't know how to do that."
