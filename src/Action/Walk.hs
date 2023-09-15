{-# LANGUAGE FlexibleContexts #-}

module Action.Walk (walkAction) where

import Action.Look(look)
import Common (dontKnowHowToDoThat, outF)
import Control.Error ((??), fromMaybe, headMay, hoistEither, runExceptT)
import Control.Lens ((.=), (^.), use)
import Control.Monad.State.Lazy (MonadState)
import Data.List (find)
import qualified Data.Map.Strict as M
import Parser (parseDirM)
import Types
import Util (hoistL, hoistR)

walkAction :: MonadState Game m => [Input] -> m (Either String String)
walkAction inputs = runExceptT $ do
    target     <- headMay inputs ?? goWhere
    mDir       <- parseDirM $ target ^. normal
    dir'       <- mDir ?? dontKnowHowToDoThat
    currentLoc <- use loc
    locMap     <- use locs
    conns'     <- use connections
    let move = Movement currentLoc dir'
    _ <- if isLocked move conns' then hoistL theDoorIsLocked else hoistR ()
    let next = resolveMove (Movement currentLoc dir') conns'
    loc .= next
    out <- Action.Look.look
    hoistEither $ Right out

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
