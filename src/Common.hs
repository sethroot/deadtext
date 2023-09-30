{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Common where

import Control.Lens ((^.), use)
import Control.Monad.State.Lazy (MonadState)
import Data.Char (toLower)
import Data.Text as T(Text, uncons)
import Types
    ( Game,
      Container,
      Item,
      ItemLocation(ItemInv, ItemLoc),
      Npc,
      HasLoc(..),
      HasItems(items) )
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

notHolding :: Item -> Bool
notHolding = not . inInventory

inventory :: MonadState Game m => m [Item]
inventory = do
    items' <- use items
    pure $ Prelude.filter inInventory items'

indefArt :: Text -> Text
indefArt t = isVowel ? "an" $ "a"
    where
        isVowel =
            let head' = maybe ' ' (toLower . fst) $ T.uncons t
            in head' `elem` ['a', 'e', 'i', 'o', 'u']

period :: Text -> Text
period  = (<> ".")

outF :: Text
outF = "Something has gone horribly wrong."

dontKnowHowToDoThat :: Text
dontKnowHowToDoThat = "You don't know how to do that."
