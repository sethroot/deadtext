{-# LANGUAGE FlexibleContexts #-}

module Action.Look (lookAction) where

import Common (containerIsHere, indefArt, itemIsHere, npcIsHere, period)
import Control.Applicative (Alternative((<|>)))
import Control.Error
    ( (??)
    , MaybeT(MaybeT, runMaybeT)
    , fromMaybe
    , headMay
    , hoistMaybe
    , runExceptT
    )
import Control.Lens ((^.), use, view)
import Control.Monad.State.Lazy (MonadState)
import Data.Char (toLower)
import Data.List (intercalate, intersperse, partition)
import qualified Data.Map.Strict as M
import Parser
    (parseContObjM, parseInvObjM, parseItemObjM, parseNpcObjM, parseRecM)
import Types
import Util ((?))

lookAction :: MonadState Game m
           => [Input]
           -> m (Either String String)
lookAction [] = do
    out <- Action.Look.look
    pure . Right $ out
lookAction ((Input _ "at") : inputs) = do
    out <- lookAt inputs
    pure . Right $ fromMaybe dontSeeThat out
lookAction ((Input _ "in") : target : _) = do
    lookIn target
lookAction _ = pure . Right $ lookWhere

dontSeeThat :: String
dontSeeThat = "You don't see that here."

lookWhere :: String
lookWhere = "Look where?"

look :: MonadState Game m => m String
look = do
    npcs'       <- use npcs
    locUid      <- use loc
    locMap      <- use locs
    conns'      <- use connections
    containers' <- use containers
    itemsInLoc' <- itemsInLoc
    let loc'          = M.lookup locUid locMap
    let locDesc       = maybe "" (^. lookDesc) loc' :: String
    let exitsDesc     = exitsInLoc locUid conns'
    let npcDesc       = go $ npcsInLoc locUid npcs'
    let containerDesc = go $ containersInLoc locUid containers'
    let itemDesc      = go itemsInLoc'
    let descs         = [locDesc, exitsDesc, npcDesc, containerDesc, itemDesc]
    pure $ formatMulti descs
    where go = fromMaybe ""

npcsInLoc :: UID -> [Npc] -> Maybe String
npcsInLoc locUid npcs' = do
    let npcsHere = filter (\npc -> locUid == npc ^. loc) npcs'
    if null npcsHere
        then Nothing
        else Just . mconcat . intersperse "\n" . map descNpc $ npcsHere

descNpc :: Npc -> String
descNpc npc = _lookDesc npc
    where _lookDesc = (npc ^. alive) ? seeNpc $ seeCorpse

seeNpc :: Npc -> String
seeNpc npc = period . unwords $ ["You see", npc ^. name, "here"]

seeCorpse :: Npc -> String
seeCorpse npc =
    period . unwords $ ["You see", npc ^. name ++ "'s corpse lying motionless"]

containersInLoc :: UID -> [Container] -> Maybe String
containersInLoc loc' _containers = do
    let containersHere = filter (\c -> c ^. loc == loc') _containers
    if null containersHere
        then Nothing
        else
            let out = mconcat . intersperse "\n" . map _desc $ containersHere
            in Just out
    where
        _desc c =
            (c ^. cState == Closed) ? containerHere c $ openContainerHere c

containerHere :: Container -> String
containerHere container = container ^. Types.look

openContainerHere :: Container -> String
openContainerHere container =
    period . unwords $ ["There is an open", container ^. name, "here"]

itemsInLoc :: MonadState Game m => m (Maybe String)
itemsInLoc = do
    items' <- use items
    loc'   <- use loc
    let itemsHere = filter (\_item -> ItemLoc loc' == _item ^. loc) items'
    if null itemsHere
        then pure Nothing
        else pure $ Just . mconcat . intersperse "\n" . map itemHere $ itemsHere

itemHere :: Item -> String
itemHere _item = period . unwords $ ["There is a", _item ^. name, "here"]

exitsInLoc :: UID -> [Connection] -> String
exitsInLoc _loc conns =
    let pathsStartingHere = filter (\c -> (c ^. start) == _loc) conns
        partitioned = partition (\p -> p ^. method == ConnectionMethodPath) pathsStartingHere
        paths = fst partitioned
        doors = snd partitioned
        desc' = fmap describePath
        exits = desc' paths <> [""] <> desc' doors
    in mconcat . intersperse "\n" $ exits

describePath :: Connection -> String
describePath (Connection _ dir' _ method' _) =
    case method' of
        ConnectionMethodPath -> pathDesc dir' 
        ConnectionMethodDoor -> doorDesc dir'

pathDesc :: Direction -> String
pathDesc _dir = period . unwords $ ["There is a path going", show _dir]

doorDesc :: Direction -> String
doorDesc _dir = period . unwords $ ["You see a door to the", show _dir]

formatMulti :: [String] -> String
formatMulti = intercalate "\n\n" . filter (not . null)

lookAt :: MonadState Game m => [Input] -> m (Maybe String)
lookAt inputs = runMaybeT $ do
    fst' <- hoistMaybe . headMay $ inputs
    let target = fst' ^. normal
    invItem <- parseInvObjM target
    item'   <- parseRecM parseItemObjM inputs
    npc     <- parseNpcObjM target
    cont    <- parseContObjM target
    obj     <- hoistMaybe $ invItem <|> item' <|> npc <|> cont
    case obj of
        ObjInv  _item     -> pure $ _item ^. desc
        ObjNpc  _npc      -> MaybeT $ lookAtNpc _npc
        ObjItem _item     -> MaybeT $ lookAtItem _item
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
    _items           <- use items
    let items'        = filter (\i -> i ^. loc == ItemContainer (cont ^. uid)) _items
    let containerDesc = Just (cont ^. desc)
    if not (null items') && cont ^. trans == Transparent
        then do
            let contName = fmap toLower $ cont ^. name
            let
                itemDesc = concatMap
                    (\i -> seeInTransparentContainer (i ^. name) contName)
                    items'
            pure . Just $ intercalate "\n\n" [cont ^. desc, itemDesc]
        else pure $ containerIsHere' ? containerDesc $ Nothing

lookIn :: MonadState Game m => Input -> m (Either String String)
lookIn _input = runExceptT $ do
    loc'        <- use loc
    containers' <- use containers
    let _pred     = containerPredicate _input loc'
    let container = headMay . filter _pred $ containers'
    let target    = _input ^. normal
    container' <- container ?? dontSeeObject target
    let cState' = container' ^. cState
    let trans'  = container' ^. trans
    lookInContainer container' cState' trans'

lookInContainer :: MonadState Game m
                => Container
                -> ContainerState
                -> ContainerTransparency
                -> m String
lookInContainer cont Closed Opaque = do
    pure . containerIsClosed $ cont ^. name
lookInContainer cont Closed Transparent = do
    items' <- use items
    let _item    = headMay . filter (itemPredicate cont) $ items'
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
itemPredicate container _item =
    _item ^. loc == ItemContainer (container ^. uid)

dontSeeObject :: String -> String
dontSeeObject object =
    period . unwords $ ["You don't see", indefArt object, object, "here"]

containerIsClosed :: String -> String
containerIsClosed container =
    period . unwords $ ["The", container, "is closed"]

containerIsEmpty :: String -> String
containerIsEmpty container = period . unwords $ ["The", container, "is empty"]

seeInContainer :: String -> String -> String
seeInContainer _item container =
    period . unwords $ ["You see", indefArt _item, _item, "in the", container]

seeInTransparentContainer :: String -> String -> String
seeInTransparentContainer _item container =
    let out = ["Inside the", container, "you can see", indefArt _item, _item]
    in period . unwords $ out
