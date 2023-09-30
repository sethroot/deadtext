{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Action.Use (useAction) where

import Control.Error ((!?), headMay, runExceptT)
import Control.Lens ((.=), Ixed(ix), (^.), use)
import Control.Monad.State.Lazy (MonadState)
import Data.List (elemIndex, find)
import Data.Maybe (fromJust)
import qualified Data.Text as T
import Parser (recParseInvObj)
import Types
import Util (hoistR)

class Usable a where
    doUse :: (MonadState Game m) => a -> m T.Text

instance Usable Obj where
    doUse (ObjInv item') = case item' ^. uid of
        "wood-side-apt-key" -> useWoodSideApartmentsKey item'
        _                   -> undefined
    doUse _ = undefined

useAction :: MonadState Game m => [Input] -> m (Either T.Text T.Text)
useAction []     = pure . Right $ "Use what?"
useAction inputs = runExceptT $ do
    item'  <- recParseInvObj inputs !? doNotHaveItem
    result <- doUse item'
    hoistR result

doNotHaveItem :: T.Text
doNotHaveItem = "You do not have that item."

useWoodSideApartmentsKey :: MonadState Game m => Item -> m T.Text
useWoodSideApartmentsKey item' = do
    let uses' = item' ^. uses
    let key   = headMay uses'
    case key of
        Just (Key useLoc accessLoc) -> do
            loc' <- use loc
            if loc' == useLoc
                then do
                    conns <- use connections
                    let conn  = fromJust . find (\c -> c ^. dest == accessLoc) $ conns
                    openConnection conn
                    pure "You unlock the Wood Side Apartments main door"
                else pure ""
        _ -> undefined

openConnection :: MonadState Game m => Connection -> m ()
openConnection c = do
    conns <- use connections
    let index  = elemIndex c conns
    let index' = fromJust index
    connections . ix index' . access .= ConnectionOpen
