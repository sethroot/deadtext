module Msg where

import           Control.Lens                   ( (^.) )
import           Data.Char                      ( toLower )
import           Types                          ( Container
                                                , Direction
                                                , HasName(name)
                                                , Item
                                                , Npc
                                                )
import           Util                           ( (?) )

dontSeeThat :: String
dontSeeThat = "You don't see that here."

dontSeeTarget :: String -> String
dontSeeTarget target = "You don't see " ++ target ++ " here."

dropObject :: String -> String
dropObject object = "You drop the " ++ object ++ "."

dontHaveObject :: String -> String
dontHaveObject object = "You do not have a " ++ object ++ "."

walkDescDefault :: String
walkDescDefault = "You are unsure where you are.."

talkCorpse :: Npc -> String
talkCorpse npc = npc ^. name ++ "'s corpse has nothing to say to you."

indefArt :: String -> String
indefArt s = isVowel ? "an" $ "a"
    where isVowel = toLower (head s) `elem` ['a', 'e', 'i', 'o', 'u']

