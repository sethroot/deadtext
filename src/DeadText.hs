{-# LANGUAGE FlexibleContexts #-}

module DeadText (deadText) where

import Action (Action(..), lookAction, processAction)
import Control.Lens ((.=))
import Control.Monad (forever, void)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State.Lazy (MonadIO, MonadState)
import Control.Monad.Trans.Reader (ReaderT(runReaderT))
import Control.Monad.Trans.State.Lazy (StateT(runStateT))
import Data (initState, initEnv)
import Data.Functor ((<&>))
import Load (loadExternal, loadInternal)
import Parser (normalizeInput, parseRawInput)
import System.Environment (getArgs)
import System.IO (hFlush, stdout)
import Types

deadText :: IO ()
deadText = void $ runStateT (runReaderT game initEnv) initState

game :: App ()
game = do
    args <- liftIO getArgs
    processArgs args
    lookAction []
    forever $ do
        printPrompt
        parsed <- liftIO getLine <&> parseInput
        input .= parsed
        tokenize parsed >>= exec

processArgs :: (MonadState Game m, MonadIO m) => [String] -> m ()
processArgs ("noload" : _) = loadInternal
processArgs ("-n"     : _) = loadInternal
processArgs (file     : _) = loadExternal file
processArgs _              = loadExternal "example"

printPrompt :: MonadIO m => m ()
printPrompt = do
    liftIO $ putStr ":> "
    liftIO $ hFlush stdout

parseInput :: String -> [Input]
parseInput input' =
    let
        raw'        = parseRawInput input'
        normalized  = normalizeInput raw'
    in zipWith Input raw' normalized
    -- liftIO $ dumpInputs raw

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
