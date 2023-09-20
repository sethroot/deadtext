{-# LANGUAGE FlexibleContexts #-}

module Action.Walk (walkAction) where

import Action.Look (look)
import Common (dontKnowHowToDoThat)
import Control.Error ((??), fromMaybe, headMay, runExceptT)
import Control.Lens ((.=), (^.), use)
import Control.Monad.State.Lazy (MonadState)
import Data.List (find)
import Parser (parseDirM)
import Types
import Util (hoistL, hoistR)

walkAction :: MonadState Game m => [Input] -> m (Either String String)
walkAction inputs = runExceptT $ do
    target <- headMay inputs ?? goWhere
    mDir   <- parseDirM $ target ^. normal
    dir'   <- mDir ?? dontKnowHowToDoThat
    loc'   <- use loc
    conns' <- use connections
    let move = Movement loc' dir'
    _ <- if isLocked move conns' then hoistL theDoorIsLocked else hoistR ()
    let next = resolveMove (Movement loc' dir') conns'
    _ <- if loc' == next then hoistL youCantGoThatWay else hoistR ()
    loc .= next
    Action.Look.look >>= hoistR

isLocked :: Movement -> [Connection] -> Bool
isLocked move conns = maybe
    False
    ((== ConnectionLocked) . (^. access))
    (maybeConnFromMove move conns)

resolveMove :: Movement -> [Connection] -> UID
resolveMove move conns =
    let
        current = move ^. start
        dest'   = maybeLocFromMove move conns
    in fromMaybe current dest'

maybeLocFromMove :: Movement -> [Connection] -> Maybe UID
maybeLocFromMove move = fmap (^. dest) . maybeConnFromMove move

maybeConnFromMove :: Movement -> [Connection] -> Maybe Connection
maybeConnFromMove (Movement s d) = find predicate
    where
        predicate = \c ->
            let
                start' = c ^. start
                dir'   = c ^. dir
            in s == start' && d == dir'

goWhere :: String
goWhere = "Go where?"

theDoorIsLocked :: String
theDoorIsLocked = "The door is locked."

youCantGoThatWay :: String
youCantGoThatWay = "You can't go that way."
