{-# LANGUAGE FlexibleContexts #-}

module Action.Look where

import           Common                         ( containerIsHere
                                                , itemIsHere
                                                , npcIsHere
                                                )
import           Control.Applicative            ( Alternative((<|>)) )
import           Control.Error                  ( MaybeT(MaybeT, runMaybeT)
                                                , fromMaybe
                                                , headMay
                                                , hoistEither
                                                , hoistMaybe
                                                , runExceptT
                                                )
import           Control.Lens                   ( (^.)
                                                , use
                                                )
import           Control.Monad.IO.Class         ( MonadIO(..) )
import           Control.Monad.State.Lazy       ( MonadIO(..)
                                                , MonadState
                                                )
import           Control.Monad.Trans.Maybe      ( MaybeT(MaybeT, runMaybeT) )
import           Data.Char                      ( toLower )
import           Data.List                      ( intercalate
                                                , intersperse
                                                )
import qualified Data.Map.Strict               as M
import           Data.Maybe                     ( fromMaybe )
import           Msg                            ( indefArt )
import           Parsing                        ( parseContObj
                                                , parseInvObj
                                                , parseItemObj
                                                , parseNpcObj
                                                )
import           Safe                           ( headMay )
import           Types
import           Util                           ( (?) )

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
descNpc npc = lookDesc npc
    where lookDesc = (npc ^. alive) ? seeNpc $ seeCorpse

seeNpc :: Npc -> String
seeNpc npc = "You see " ++ npc ^. name ++ " here."

seeCorpse :: Npc -> String
seeCorpse npc = "You see " ++ npc ^. name ++ "'s corpse lying motionless."

containersInLoc :: MonadState Game m => UID -> [Container] -> m (Maybe String)
containersInLoc loc' containers = do
    let containersHere = filter (\c -> c ^. loc == loc') containers
    if null containersHere
        then pure Nothing
        else
            let out = mconcat . intersperse "\n" . map desc $ containersHere
            in  pure $ Just out
  where
    desc c = (c ^. cState == Closed) ? containerHere c $ openContainerHere c

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
    let itemsHere = filter (\item -> ItemLoc loc' == item ^. loc) items'
    if null itemsHere
        then pure Nothing
        else pure $ Just . mconcat . intersperse "\n" . map itemHere $ itemsHere

itemHere :: Item -> String
itemHere item = "There is a " ++ item ^. name ++ " here."

pathsInLoc :: UID -> [Connection] -> String
pathsInLoc loc conns =
    let paths = pathsInLoc' loc conns
    in  mconcat . intersperse "\n" $ map (pathGoing . fst) paths

pathsInLoc' :: UID -> [Connection] -> [(Direction, UID)]
pathsInLoc' loc conns =
    let paths = filter (\c -> (c ^. start) == loc) conns
    in  zip (map (^. dir) paths) (map (^. dest) paths)

pathGoing :: Direction -> String
pathGoing dir = "There is a path going " ++ show dir

formatMulti :: [String] -> String
formatMulti = mconcat . intersperse "\n\n" . filter (not . null)

-- lookAt

lookAt :: MonadState Game m => Input -> m (Maybe String)
lookAt input = runMaybeT $ do
    let target = input ^. normal
    invItem <- parseInvObj target
    item    <- parseItemObj target
    npc     <- parseNpcObj target
    cont    <- parseContObj target
    obj     <- hoistMaybe $ invItem <|> item <|> npc <|> cont
    case obj of
        ObjInv  item      -> pure $ item ^. desc
        ObjNpc  npc       -> MaybeT $ lookAtNpc npc
        ObjItem item      -> MaybeT $ lookAtItem item
        ObjCont container -> MaybeT $ lookAtContainer container

lookAtNpc :: MonadState Game m => Npc -> m (Maybe String)
lookAtNpc npc = do
    npcIsHere' <- npcIsHere npc
    pure $ npcIsHere' ? Just (npc ^. desc) $ Nothing

lookAtItem :: MonadState Game m => Item -> m (Maybe String)
lookAtItem item = do
    itemIsHere' <- itemIsHere item
    pure $ itemIsHere' ? Just (item ^. desc) $ Nothing

lookAtContainer :: MonadState Game m => Container -> m (Maybe String)
lookAtContainer cont = do
    containerIsHere' <- containerIsHere cont
    items            <- use items
    let items'        = filter (\i -> i ^. loc == ItemContainer (cont ^. uid)) items
    let containerDesc = Just (cont ^. desc)
    if not (null items') && cont ^. trans
        then do
            let cName = fmap toLower $ cont ^. name
            let inside        = "Inside the " ++ cName ++ " you can see a "
            let itemDesc      = concatMap (\i -> inside ++ i ^. name) $ items'
            pure . Just $ intercalate "\n\n" [cont ^. desc, itemDesc]
        else pure $ containerIsHere' ? containerDesc $ Nothing

-- lookIn

lookIn :: MonadState Game m => Input -> m (Either String String)
lookIn input = runExceptT $ do
    loc'        <- use loc
    containers' <- use containers
    let pred      = containerPredicate input loc'
    let container = headMay . filter pred $ containers'
    container' <- case container of
        Nothing -> do
            let out = dontSeeObject $ input ^. normal
            hoistEither $ Left out
        Just c -> hoistEither $ Right c
    if container' ^. cState == Closed
        then do
            let out = containerIsClosed $ container' ^. name
            hoistEither $ Left out
        else hoistEither $ Right ()
    items' <- use items
    let item = headMay . filter (itemPredicate container') $ items'
    case item of
        Nothing -> do
            let out = containerIsEmpty $ container' ^. name
            hoistEither $ Left out
        Just item' -> do
            let itemName = item' ^. name
            let out      = seeInContainer itemName (container' ^. name)
            hoistEither $ Right out

containerPredicate :: Input -> UID -> Container -> Bool
containerPredicate input loc' container = nameMatch && locMatch
  where
    nameMatch = fmap toLower (container ^. name) == input ^. normal
    locMatch  = container ^. loc == loc'

itemPredicate :: Container -> Item -> Bool
itemPredicate container item = item ^. loc == ItemContainer (container ^. uid)

dontSeeObject :: String -> String
dontSeeObject object = "You don't see a " ++ object ++ " here."

containerIsClosed :: String -> String
containerIsClosed container = "The " ++ container ++ " is closed."

containerIsEmpty :: String -> String
containerIsEmpty container = "The " ++ container ++ " is empty."

seeInContainer :: String -> String -> String
seeInContainer item container =
    "You see " ++ indefArt item ++ " " ++ item ++ " in the " ++ container ++ "."
