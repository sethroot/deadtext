{-# LANGUAGE FlexibleContexts #-}
-- {-# OPTIONS_GHC -Wno-unused-imports #-}

module DeadText
    ( deadText
    ) where

import           Action                         ( lookAction
                                                , processAction
                                                )
import           Control.Lens                   ( (.=) )
import           Control.Monad                  ( forever
                                                , void
                                                )
import           Control.Monad.IO.Class         ( liftIO )
import           Control.Monad.State.Lazy       ( MonadIO
                                                , MonadState
                                                )
import           Control.Monad.Trans.State.Lazy ( StateT(runStateT) )
import           Data                           ( initState )
import           Load                           ( loadExternal
                                                , loadInternal
                                                )
import           Parser                         ( normalizeInput
                                                , parseRawInput
                                                )
import           System.Environment             ( getArgs )
import           System.IO                      ( hFlush
                                                , stdout
                                                )
import           Types                          ( Game
                                                , GameLoop
                                                , HasInput(input)
                                                , Input(Input)
                                                )

deadText :: IO ()
deadText = void $ runStateT game initState

game :: GameLoop
game = do
    args <- liftIO getArgs
    processArgs args
    lookAction []
    forever $ printPrompt >> liftIO getLine >>= parseInput >>= tokenize >>= exec
    pure ()

processArgs :: [String] -> GameLoop
processArgs ("noload" : _) = loadInternal
processArgs ("-n"     : _) = loadInternal
processArgs (file     : _) = loadExternal file
processArgs _              = loadExternal "example"

printPrompt :: MonadIO m => m ()
printPrompt = do
    liftIO $ putStr ":> "
    liftIO $ hFlush stdout

parseInput :: MonadState Game m => String -> m [Input]
parseInput input' = do
    let raw        = parseRawInput input'
    let normalized = normalizeInput raw
    let input'     = zipWith Input raw normalized
    input .= input'
    pure input'
    -- liftIO $ dumpInputs raw

data Action = Action Input (Maybe Input) [Input]

tokenize :: Monad m => [Input] -> m (Action)
tokenize input' = do
    let action = head input'
    let arg =
            if length input' > 1 then Just . head . tail $ input' else Nothing
    let args = tail input'
    pure $ Action action arg args

exec :: (MonadState Game m, MonadIO m) => Action -> m ()
exec (Action action arg args) = do
    liftIO $ putStrLn ""
    processAction action arg args
    liftIO $ putStrLn ""
