{-# LANGUAGE FlexibleContexts #-}

module DeadText where

import Action (Action(..), lookAction, processAction)
import Control.Lens ((.=))
import Control.Monad (forever, void)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State.Lazy (MonadIO, MonadState)
import Control.Monad.Trans.Reader (ReaderT(runReaderT))
import Control.Monad.Trans.State.Lazy (StateT(runStateT))
import Data (initState, initEnv)
import Data.Functor ((<&>))
import Load (loadInternal)
import Parser (normalizeInput, parseRawInput)
import System.Environment (getArgs)
import System.IO (hFlush, stdout)
import Types
import Util (enumerate)

deadText :: IO ()
deadText = void $ runStateT (runReaderT game initEnv) initState

game :: App ()
game = do
    args <- liftIO getArgs
    processArgs args
    lookAction []
    forever $ do
        printPrompt
        parsed <- liftIO getLine >>= parseInput
        input .= parsed
        tokenize parsed >>= exec

processArgs :: (MonadState Game m, MonadIO m) => [String] -> m ()
processArgs ("noload" : _) = loadInternal
processArgs ("-n"     : _) = loadInternal
-- processArgs (file     : _) = loadExternal file
-- processArgs _              = loadExternal "example"

printPrompt :: MonadIO m => m ()
printPrompt = do
    liftIO $ putStr ":> "
    liftIO $ hFlush stdout

parseInput :: MonadIO m => String -> m [Input]
parseInput input' = do
    let rawInputs = parseRawInput input'
    let normalizedInputs = normalizeInput rawInputs
    let inputs = zipWith Input rawInputs normalizedInputs
    -- liftIO . print . enumerate $ normalizedInputs
    -- liftIO . print $ inputs
    pure inputs

tokenize :: Monad m => [Input] -> m Action
tokenize input' = do
    let action = head input'
    let args   = tail input'
    pure $ Action action args

exec :: (MonadState Game m, MonadIO m) => Action -> m ()
exec action = do
    liftIO $ putStrLn ""
    processAction action
    liftIO $ putStrLn ""
