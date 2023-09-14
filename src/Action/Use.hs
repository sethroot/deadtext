{-# LANGUAGE FlexibleContexts #-}

module Action.Use where

import Control.Error ((!?), headMay, runExceptT)
import Control.Lens ((^.), use, elemIndexOf, (.=), Ixed (ix), (&), (.~))
import Control.Monad.State.Lazy (MonadState (put, get))
import Data
import Parser (recParseInvObj)
import Types
import Util (hoistL, hoistR)
import Data.List (find, elemIndex)
import Data.Maybe (fromJust)

class Usable a where
    doUse :: (MonadState Game m) => a -> m String

instance Usable Obj where
    doUse (ObjInv item') = case item' ^. uid of
        "wood-side-apt-key" -> useWoodSideApartmentsKey item'
        _                   -> undefined
    doUse _ = undefined

useAction :: MonadState Game m => [Input] -> m (Either String String)
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
                    -- let index = elemIndex c' conns
                    -- let index' = fromJust index
                    -- game <- get
                    -- _ <- put $ game & conns . ix index' . access .= ConnectionOpen
                    openConnection c'
                    pure "You unlock the Wood Side Apartments main door"
                else pure ""
        _ -> undefined
    -- out <- runExceptT $ do
    --     loc' <- use loc
    --     _    <- if loc' == woodSideExtWestUid
    --         then hoistR ()
    --         else hoistL "You can't use that here."
    --     conns <- use connections
    --     let conn = filter (\c -> c ^. start == woodSideExtWestUid) conns
    --     pure ""
    -- pure ""

openConnection :: MonadState Game m => Connection -> m ()
openConnection c = do
      conns <- use connections
      let index = elemIndex c conns
      let index' = fromJust index
      connections . ix index' . access .= ConnectionOpen
