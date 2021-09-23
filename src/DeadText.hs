{-# LANGUAGE FlexibleContexts #-}
-- {-# OPTIONS_GHC -Wno-unused-imports #-}

module DeadText
    ( deadText
    ) where

import           Action                         ( Action(..)
                                                , lookAction
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
import           Data.Functor                   ( (<&>) )
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
    forever $ do
        printPrompt
        parsed <- liftIO getLine <&> parseInput
        input .= parsed
        tokenize parsed >>= exec
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

parseInput :: String -> [Input]
parseInput input' =
    let raw        = parseRawInput input'
        normalized = normalizeInput raw
    in  zipWith Input raw normalized
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
