{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module DeadText where

import Action (Action(..), lookAction, processAction)
import Control.Lens ((<.=))
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
    _ <- liftIO getArgs >>= processArgs . fmap T.pack
    lookAction [] >>= printE
    forever gameLoop

gameLoop :: (MonadState Game m, MonadIO m) => m ()
gameLoop = do
    printPrompt
    liftIO getLine >>= parse >>= store >>= tokenize >>= execute

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

parse :: (Applicative m) => String -> m [Input]
parse input' = do
    let rawInputs        = parseRawInput . T.pack $ input'
    let normalizedInputs = normalizeInput rawInputs
    let inputs           = zipWith Input rawInputs normalizedInputs
    pure inputs

store :: MonadState Game m => [Input] -> m [Input]
store = (input <.=)

tokenize :: Applicative m => [Input] -> m Action
tokenize input' = do
    let action = head input'
    let args   = tail input'
    pure $ Action action args

execute :: (MonadState Game m, MonadIO m) => Action -> m ()
execute action = do
    liftIO $ putStrLn ""
    processAction action
    liftIO $ putStrLn ""
