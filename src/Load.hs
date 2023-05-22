{-# LANGUAGE FlexibleContexts #-}

module Load where

import Control.Error (MaybeT(runMaybeT), hoistMaybe)
import Control.Monad.State.Lazy (MonadIO(..), MonadState(get, put))
import Data (setState)
import Data.Aeson (decode)
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy as BL
import Data.Maybe (fromJust)
-- import Ext (GameExt, toGame)
import System.IO (IOMode(ReadMode), openFile)
import Types (Game)

loadInternal :: (MonadState Game m, MonadIO m) => m () 
loadInternal = do
    Data.setState
    game <- get
    -- liftIO $ exportGame game

    liftIO . putStrLn $ ""
    liftIO . putStrLn $ "Running against internal config"
    liftIO . putStrLn $ ""

-- loadExternal :: (MonadState Game m, MonadIO m) => String -> m () 
-- loadExternal file = do
--     game <- loadGame file
--     liftIO . putStrLn $ ""
--     -- liftIO $ printGame $ fromJust game

--     put $ fromJust game

-- loadGame :: (MonadState Game m, MonadIO m) => String -> m (Maybe Game)
-- loadGame file = runMaybeT $ do
--     let path = "json/" ++ file ++ ".json"
--     handle   <- liftIO $ openFile path ReadMode
--     contents <- liftIO $ BL.hGetContents handle
--     let gameExt = decode contents :: Maybe GameExt
--     case gameExt of
--         Nothing      -> hoistMaybe Nothing
--         Just gameExt -> do
--             game <- toGame gameExt
--             hoistMaybe $ Just game

importRaw :: IO (Maybe Game)
importRaw = do
    pure Nothing
    -- handle   <- openFile "json/in.json" ReadMode
    -- contents <- BL.hGetContents handle
    -- -- BL.putStr contents

    -- let game = decode contents :: Maybe Game
    -- -- liftIO $ printGame $ fromJust game

    -- pure game

exportRaw :: Game -> IO ()
exportRaw state = do
    pure ()
    -- BL.writeFile "json/out.json" $ encodePretty state
