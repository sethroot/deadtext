{-# LANGUAGE FlexibleContexts #-}

module Action.Use (useAction) where

import Control.Error ((!?), headMay, runExceptT)
import Control.Lens ((^.), use, (.=), Ixed (ix))
import Data
import Parser (recParseInvObj)
import Types
import Util (hoistR)
import Data.List (find, elemIndex)
import Data.Maybe (fromJust)
import Control.Monad.State.Lazy (MonadState)

class Usable a where
    doUse :: (MonadState Game m) => a -> m String

instance Usable Obj where
    doUse (ObjInv item') = case item' ^. uid of
        "wood-side-apt-key" -> useWoodSideApartmentsKey item'
        _                   -> undefined
    doUse _ = undefined

useAction :: MonadState Game m => [Input] -> m (Either String String)
useAction [] = pure . Right $ "Use what?"
useAction inputs = runExceptT $ do
    item'  <- recParseInvObj inputs !? doNotHaveItem
    result <- doUse item'
    hoistR result

doNotHaveItem :: String
doNotHaveItem = "You do not have that item."

useWoodSideApartmentsKey :: MonadState Game m => Item -> m String
useWoodSideApartmentsKey item' = do
    let uses' = item' ^. uses
    let key   = headMay uses'
    case key of
        Just (Key useLoc accessLoc) -> do
            loc' <- use loc
            if loc' == useLoc
                then do
                    conns <- use connections
                    let
                        c =
                            find (\c -> c ^. dest == accessLoc)
                                $ conns
                    let c' = fromJust c
                    openConnection c'
                    pure "You unlock the Wood Side Apartments main door"
                else pure ""
        _ -> undefined

openConnection :: MonadState Game m => Connection -> m ()
openConnection c = do
      conns <- use connections
      let index = elemIndex c conns
      let index' = fromJust index
      connections . ix index' . access .= ConnectionOpen
