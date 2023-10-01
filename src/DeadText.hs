{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module DeadText where

import Action (Action(..), lookAction, processAction)
import Control.Lens ((.=))
import Control.Monad (forever, void)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State.Lazy (MonadIO, MonadState)
import Control.Monad.Trans.Reader (ReaderT(runReaderT))
import Control.Monad.Trans.State.Lazy (StateT(runStateT))
import Data (initEnv, initState)
import qualified Data.Text as T
import Load (loadInternal)
import Parser (normalizeInput, parseRawInput)
import System.Environment (getArgs)
import System.IO (hFlush, stdout)
import Types
import Util (printE)

type App = ReaderT Env (StateT Game IO)

deadText :: IO ()
deadText = void $ runStateT (runReaderT game initEnv) initState

game :: App ()
game = do
    args <- liftIO getArgs
    let txtArgs = fmap T.pack args
    processArgs txtArgs
    lookAction [] >>= printE
    forever gameLoop 

gameLoop :: (MonadState Game m, MonadIO m) => m ()
gameLoop = do
    printPrompt
    liftIO getLine
        >>= parseInput
        >>= storeInput
        >>= tokenizeInputAsAction
        >>= executeAction

processArgs :: (MonadState Game m, MonadIO m) => [T.Text] -> m ()
processArgs ("noload" : _) = loadInternal
processArgs ("-n"     : _) = loadInternal
processArgs _              = loadInternal
-- processArgs (file     : _) = loadExternal file
-- processArgs _              = loadExternal "example"

printPrompt :: MonadIO m => m ()
printPrompt = do
    liftIO $ putStr ":> "
    liftIO $ hFlush stdout

parseInput :: MonadIO m => String -> m [Input]
parseInput input' = do
    let rawInputs        = parseRawInput . T.pack $ input'
    let normalizedInputs = normalizeInput rawInputs
    let inputs           = zipWith Input rawInputs normalizedInputs
    -- liftIO . print . enumerate $ normalizedInputs
    -- liftIO . print $ inputs
    pure inputs

storeInput :: MonadState Game m => [Input] -> m [Input]
storeInput input' = do
    input .= input'
    pure input'

tokenizeInputAsAction :: Monad m => [Input] -> m Action
tokenizeInputAsAction input' = do
    let action = head input'
    let args   = tail input'
    pure $ Action action args

executeAction :: (MonadState Game m, MonadIO m) => Action -> m ()
executeAction action = do
    liftIO $ putStrLn ""
    processAction action
    liftIO $ putStrLn ""
