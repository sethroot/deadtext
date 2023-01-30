{-# LANGUAGE FlexibleContexts #-}

module Action.Look (lookAction) where

import Common (containerIsHere, indefArt, itemIsHere, npcIsHere)
import Control.Applicative (Alternative((<|>)))
import Control.Error
    ( MaybeT(MaybeT, runMaybeT)
    , fromMaybe
    , headMay
    , hoistEither
    , hoistMaybe
    , runExceptT
    )
import Control.Lens ((^.), use, view)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.State.Lazy (MonadState)
import Data.Char (toLower)
import Data.List (intercalate, intersperse)
import qualified Data.Map.Strict as M
import Parser (parseContObj, parseInvObj, parseItemObj, parseNpcObj)
import Types
import Util ((?))

lookAction :: (MonadState Game m, MonadIO m) => [Input] -> m ()
lookAction [] = do
    out <- Action.Look.look
    liftIO . putStrLn $ out
lookAction ((Input _ "at") : target : _) = do
    out <- lookAt target
    liftIO . putStrLn $ fromMaybe dontSeeThat out
lookAction ((Input _ "in") : target : _) = do
    out <- lookIn target
    either printE printE out
    where printE = liftIO . putStrLn
lookAction _ = liftIO . putStrLn $ lookWhere

dontSeeThat :: String
dontSeeThat = "You don't see that here."

lookWhere :: String
lookWhere = "Look where?"

-- look

look :: MonadState Game m => m String
look = do
    locUid           <- use loc
    locMap           <- use locs
    conns'           <- use connections
    containers'      <- use containers
    npcsInLoc'       <- npcsInLoc
    containersInLoc' <- containersInLoc locUid containers'
    itemsInLoc'      <- itemsInLoc
    let loc''         = M.lookup locUid locMap
    let locDesc       = maybe "" (^. lookDesc) loc'' :: String
    let pathsDesc     = pathsInLoc locUid conns'
    let npcDesc       = fromMaybe "" npcsInLoc'
    let containerDesc = fromMaybe "" containersInLoc'
    let itemDesc      = fromMaybe "" itemsInLoc'
    let descs         = [locDesc, pathsDesc, npcDesc, containerDesc, itemDesc]
    pure $ formatMulti descs

npcsInLoc :: MonadState Game m => m (Maybe String)
npcsInLoc = do
    npcs' <- use npcs
    loc'  <- use loc
    let npcsHere = filter (\npc -> loc' == npc ^. loc) npcs'
    if null npcsHere
        then pure Nothing
        else pure $ Just . mconcat . intersperse "\n" . map descNpc $ npcsHere

descNpc :: Npc -> String
descNpc npc = _lookDesc npc
    where _lookDesc = (npc ^. alive) ? seeNpc $ seeCorpse

seeNpc :: Npc -> String
seeNpc npc = "You see " ++ npc ^. name ++ " here."

seeCorpse :: Npc -> String
seeCorpse npc = "You see " ++ npc ^. name ++ "'s corpse lying motionless."

containersInLoc :: MonadState Game m => UID -> [Container] -> m (Maybe String)
containersInLoc loc' _containers = do
    let containersHere = filter (\c -> c ^. loc == loc') _containers
    if null containersHere
        then pure Nothing
        else
            let out = mconcat . intersperse "\n" . map _desc $ containersHere
            in pure $ Just out
    where
        _desc c =
            (c ^. cState == Closed) ? containerHere c $ openContainerHere c

containerHere :: Container -> String
containerHere container = container ^. Types.look
-- containerHere container = "There is a " ++ container ^. name ++ " here."

openContainerHere :: Container -> String
openContainerHere container =
    "There is an open " ++ container ^. name ++ " here."

itemsInLoc :: MonadState Game m => m (Maybe String)
itemsInLoc = do
    items' <- use items
    loc'   <- use loc
    let itemsHere = filter (\_item -> ItemLoc loc' == _item ^. loc) items'
    if null itemsHere
        then pure Nothing
        else pure $ Just . mconcat . intersperse "\n" . map itemHere $ itemsHere

itemHere :: Item -> String
itemHere _item = "There is a " ++ _item ^. name ++ " here."

pathsInLoc :: UID -> [Connection] -> String
pathsInLoc _loc conns =
    let paths = pathsInLoc' _loc conns
    in mconcat . intersperse "\n" $ map (pathGoing . fst) paths

pathsInLoc' :: UID -> [Connection] -> [(Direction, UID)]
pathsInLoc' _loc conns =
    let paths = filter (\c -> (c ^. start) == _loc) conns
    in zip (map (^. dir) paths) (map (^. dest) paths)

pathGoing :: Direction -> String
pathGoing _dir = "There is a path going " ++ show _dir ++ "."

formatMulti :: [String] -> String
formatMulti = intercalate "\n\n" . filter (not . null)

-- lookAt

lookAt :: MonadState Game m => Input -> m (Maybe String)
lookAt _input = runMaybeT $ do
    let target = _input ^. normal
    invItem <- parseInvObj target
    _item    <- parseItemObj target
    npc     <- parseNpcObj target
    cont    <- parseContObj target
    obj     <- hoistMaybe $ invItem <|> _item <|> npc <|> cont
    case obj of
        ObjInv  _item      -> pure $ _item ^. desc
        ObjNpc  _npc       -> MaybeT $ lookAtNpc _npc
        ObjItem _item      -> MaybeT $ lookAtItem _item
        ObjCont container -> MaybeT $ lookAtContainer container

lookAtNpc :: MonadState Game m => Npc -> m (Maybe String)
lookAtNpc npc = do
    npcIsHere' <- npcIsHere npc
    pure $ npcIsHere' ? Just (npc ^. desc) $ Nothing

lookAtItem :: MonadState Game m => Item -> m (Maybe String)
lookAtItem _item = do
    itemIsHere' <- itemIsHere _item
    pure $ itemIsHere' ? Just (_item ^. desc) $ Nothing

lookAtContainer :: MonadState Game m => Container -> m (Maybe String)
lookAtContainer cont = do
    containerIsHere' <- containerIsHere cont
    _items            <- use items
    let items'        = filter (\i -> i ^. loc == ItemContainer (cont ^. uid)) _items
    let containerDesc = Just (cont ^. desc)
    if not (null items') && cont ^. trans
        then do
            let contName = fmap toLower $ cont ^. name
            let
                itemDesc = concatMap
                    (\i -> seeInTransparentContainer (i ^. name) contName)
                    items'
            pure . Just $ intercalate "\n\n" [cont ^. desc, itemDesc]
        else pure $ containerIsHere' ? containerDesc $ Nothing

-- lookIn

lookIn :: MonadState Game m => Input -> m (Either String String)
lookIn _input = runExceptT $ do
    loc'        <- use loc
    containers' <- use containers
    let _pred      = containerPredicate _input loc'
    let container = headMay . filter _pred $ containers'
    container' <- case container of
        Nothing -> do
            let out = dontSeeObject $ _input ^. normal
            hoistEither $ Left out
        Just c -> hoistEither $ Right c
    let cState' = container' ^. cState
    let trans'  = container' ^. trans
    out <- lookInContainer container' cState' trans'
    hoistEither $ Right out

lookInContainer :: MonadState Game m
                => Container
                -> ContainerState
                -> Bool
                -> m String
lookInContainer cont Closed False = do
    pure . containerIsClosed $ cont ^. name
lookInContainer cont Closed True = do
    items' <- use items
    let _item     = headMay . filter (itemPredicate cont) $ items'
    let itemName = maybe "object" (view name) _item
    let contName = cont ^. name
    pure $ seeInTransparentContainer itemName contName
lookInContainer cont Open _ = do
    items' <- use items
    let _item' = headMay . filter (itemPredicate cont) $ items'
    case _item' of
        Nothing -> do
            pure . containerIsEmpty $ cont ^. name
        Just _item'' -> do
            let itemName = _item'' ^. name
            pure $ seeInContainer itemName (cont ^. name)


containerPredicate :: Input -> UID -> Container -> Bool
containerPredicate _input loc' container = nameMatch && locMatch
    where
        nameMatch = fmap toLower (container ^. name) == _input ^. normal
        locMatch  = container ^. loc == loc'

itemPredicate :: Container -> Item -> Bool
itemPredicate container _item = _item ^. loc == ItemContainer (container ^. uid)

dontSeeObject :: String -> String
dontSeeObject object =
    "You don't see " ++ indefArt object ++ " " ++ object ++ " here."

containerIsClosed :: String -> String
containerIsClosed container = "The " ++ container ++ " is closed."

containerIsEmpty :: String -> String
containerIsEmpty container = "The " ++ container ++ " is empty."

seeInContainer :: String -> String -> String
seeInContainer _item container =
    "You see " ++ indefArt _item ++ " " ++ _item ++ " in the " ++ container ++ "."

seeInTransparentContainer :: String -> String -> String
seeInTransparentContainer _item container =
    "Inside the "
        ++ container
        ++ " you can see "
        ++ indefArt _item
        ++ " "
        ++ _item
        ++ "."
