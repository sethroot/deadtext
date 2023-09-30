{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings #-}

module Action.Look (lookAction, Action.Look.look) where

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
import Control.Lens ((^.), makeFields, use, view)
import Control.Monad.State.Lazy (MonadState)
import Data.List (intersperse, partition)
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Parser
    (parseContObjM, parseInvObjM, parseItemObjM, parseNpcObjM, parseRecM, matchesText)
import Types
import Util ((?), maybeToEither)

-- lookAction :: MonadState Game m => [Input] -> m (Either String String)
-- lookAction [] = do
--     out <- Action.Look.look
--     pure . Right $ out
-- lookAction ((Input _ "at") : inputs) = do
--     out <- lookAt inputs
--     pure . Right $ fromMaybe dontSeeThat out
-- lookAction ((Input _ "in") : target : _) = do
--     lookIn target
-- lookAction _ = pure . Right $ lookWhere

data LookParseResult a = LookParseResult 
    { _lookParseResultInput  :: [Input] 
    , _lookParseResultResult :: Maybe a
    }

data LookAction = CLookAt T.Text | CLookIn T.Text 

makeFields ''LookParseResult

parseInputs :: [Input] -> LookParseResult LookAction 
parseInputs inputs =
    case head inputs of
        _ | go "at" -> LookParseResult inputs (Just $ CLookAt ((head . tail $ inputs) ^. normal))
        _ | go "in" -> LookParseResult inputs (Just $ CLookIn ((head . tail $ inputs) ^. normal))
        _ -> LookParseResult inputs Nothing
        where go = matchesText [head inputs]

lookAction :: MonadState Game m => [Input] -> m (Either T.Text T.Text)
lookAction [] = Action.Look.look
lookAction inputs = do
    let parsed = parseInputs inputs
    let result' = parsed ^. result
    case result' of
        Just x -> case x of
            (CLookAt _) -> lookAt . tail $ inputs
            (CLookIn _) -> lookIn . head . tail $ inputs
        Nothing -> pure . Right $ lookWhere

dontSeeThat :: T.Text
dontSeeThat = "You don't see that here."

lookWhere :: T.Text
lookWhere = "Look where?"

look :: MonadState Game m => m (Either T.Text T.Text)
look = do
    npcs'       <- use npcs
    locUid      <- use loc
    locMap      <- use locs
    conns'      <- use connections
    containers' <- use containers
    itemsInLoc' <- itemsInLoc
    let loc'          = M.lookup locUid locMap
    let locDesc       = maybe "" (^. lookDesc) loc' :: T.Text
    let exitsDesc     = exitsInLoc locUid conns'
    let npcDesc       = go $ npcsInLoc locUid npcs'
    let containerDesc = go $ containersInLoc locUid containers'
    let itemDesc      = go itemsInLoc'
    let descs         = [locDesc, exitsDesc, npcDesc, containerDesc, itemDesc]
    pure . Right $ formatMulti descs
    where go = fromMaybe ""

npcsInLoc :: UID -> [Npc] -> Maybe T.Text
npcsInLoc locUid npcs' = do
    let npcsHere = filter (\npc -> locUid == npc ^. loc) npcs'
    if null npcsHere
        then Nothing
        else Just . mconcat . intersperse "\n" . map descNpc $ npcsHere

descNpc :: Npc -> T.Text
descNpc npc = _lookDesc npc
    where _lookDesc = (npc ^. alive) ? seeNpc $ seeCorpse

seeNpc :: Npc -> T.Text
seeNpc npc = period . T.unwords $ ["You see", npc ^. name, "here"]

seeCorpse :: Npc -> T.Text
seeCorpse npc =
    period
        . T.unwords
        $ ["You see", (npc ^. name) <> "'s", "corpse lying motionless"]

containersInLoc :: UID -> [Container] -> Maybe T.Text
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

containerHere :: Container -> T.Text
containerHere container = container ^. Types.look

openContainerHere :: Container -> T.Text
openContainerHere container =
    period . T.unwords $ ["There is an open", container ^. name, "here"]

itemsInLoc :: MonadState Game m => m (Maybe T.Text)
itemsInLoc = do
    items' <- use items
    loc'   <- use loc
    let itemsHere = filter (\_item -> ItemLoc loc' == _item ^. loc) items'
    if null itemsHere
        then pure Nothing
        else pure $ Just . mconcat . intersperse "\n" . map itemHere $ itemsHere

itemHere :: Item -> T.Text
itemHere _item = period . T.unwords $ ["There is a", _item ^. name, "here"]

exitsInLoc :: UID -> [Connection] -> T.Text
exitsInLoc _loc conns =
    let
        pathsStartingHere = filter (\c -> (c ^. start) == _loc) conns
        partitioned       = partition
            (\p -> p ^. method == ConnectionMethodPath)
            pathsStartingHere
        paths = fst partitioned
        doors = snd partitioned
        desc' = fmap describePath
        exits = if null doors
            then desc' paths
            else desc' paths <> [""] <> desc' doors
    in mconcat . intersperse "\n" $ exits

describePath :: Connection -> T.Text
describePath (Connection _ dir' _ method' _) = case method' of
    ConnectionMethodPath -> pathDesc dir'
    ConnectionMethodDoor -> doorDesc dir'

pathDesc :: Direction -> T.Text
pathDesc _dir =
    period . T.unwords $ ["There is a path going", T.pack $ show _dir]

doorDesc :: Direction -> T.Text
doorDesc _dir =
    period . T.unwords $ ["You see a door to the", T.pack $ show _dir]

formatMulti :: [T.Text] -> T.Text
formatMulti = T.intercalate "\n\n" . filter (not . T.null)

lookAt :: MonadState Game m => [Input] -> m (Either T.Text T.Text)
lookAt inputs = do
    out <- runMaybeT $ do
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
    pure . maybeToEither dontSeeThat $ out

lookAtNpc :: MonadState Game m => Npc -> m (Maybe T.Text)
lookAtNpc npc = do
    npcIsHere' <- npcIsHere npc
    pure $ npcIsHere' ? Just (npc ^. desc) $ Nothing

lookAtItem :: MonadState Game m => Item -> m (Maybe T.Text)
lookAtItem _item = do
    itemIsHere' <- itemIsHere _item
    pure $ itemIsHere' ? Just (_item ^. desc) $ Nothing

lookAtContainer :: MonadState Game m => Container -> m (Maybe T.Text)
lookAtContainer cont = do
    containerIsHere' <- containerIsHere cont
    _items           <- use items
    let items'        = filter (\i -> i ^. loc == ItemContainer (cont ^. uid)) _items
    let containerDesc = Just (cont ^. desc)
    if not (null items') && cont ^. trans == Transparent
        then do
            let contName = T.toLower $ cont ^. name
            let
                itemDesc = concatMap
                    (\i -> [seeInTransparentContainer (i ^. name) contName])
                    items'
            pure . Just $ T.intercalate "\n\n" $ (cont ^. desc) : itemDesc
        else pure $ containerIsHere' ? containerDesc $ Nothing

lookIn :: MonadState Game m => Input -> m (Either T.Text T.Text)
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
                -> m T.Text
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
        nameMatch = T.toLower (container ^. name) == _input ^. normal
        locMatch  = container ^. loc == loc'

itemPredicate :: Container -> Item -> Bool
itemPredicate container _item =
    _item ^. loc == ItemContainer (container ^. uid)

dontSeeObject :: T.Text -> T.Text
dontSeeObject object =
    period . T.unwords $ ["You don't see", indefArt object, object, "here"]

containerIsClosed :: T.Text -> T.Text
containerIsClosed container =
    period . T.unwords $ ["The", container, "is closed"]

containerIsEmpty :: T.Text -> T.Text
containerIsEmpty container =
    period . T.unwords $ ["The", container, "is empty"]

seeInContainer :: T.Text -> T.Text -> T.Text
seeInContainer _item container =
    period . T.unwords $ ["You see", indefArt _item, _item, "in the", container]

seeInTransparentContainer :: T.Text -> T.Text -> T.Text
seeInTransparentContainer _item container =
    let out = ["Inside the", container, "you can see", indefArt _item, _item]
    in period . T.unwords $ out
