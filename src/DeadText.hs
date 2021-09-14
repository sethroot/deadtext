{-# LANGUAGE FlexibleContexts #-}

module DeadText
    ( deadText
    ) where

import           Action                         ( lookAction
                                                , processAction
                                                )
import           Control.Error                  ( MaybeT(runMaybeT)
                                                , hoistMaybe
                                                )
import           Control.Lens                   ( (.=) )
import           Control.Monad                  ( forever
                                                , void
                                                )
import           Control.Monad.IO.Class         ( liftIO )
import           Control.Monad.State.Lazy       ( MonadIO
                                                , MonadState
                                                )
import           Control.Monad.Trans.State.Lazy ( StateT(runStateT)
                                                , get
                                                , put
                                                )
import           Data                           ( initState
                                                , setState
                                                )
import           Data.Aeson                     ( decode )
import           Data.Aeson.Encode.Pretty       ( encodePretty )
import qualified Data.ByteString.Lazy          as BL
import           Data.Maybe                     ( fromJust )
import           Load                           ( GameExt
                                                , toGame
                                                )
import           Parser                         ( normalizeInput
                                                , parseRawInput
                                                )
import           System.Environment             ( getArgs )
import           System.IO                      ( IOMode(ReadMode)
                                                , hFlush
                                                , openFile
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
    forever execGameLoop
    pure ()

processArgs :: [String] -> GameLoop
processArgs ("noload" : _) = loadInternal
processArgs ("-n"     : _) = loadInternal
processArgs (file     : _) = loadExternal file
processArgs _              = loadExternal "example"

execGameLoop :: GameLoop
execGameLoop = do
    liftIO $ putStr ":> "
    liftIO $ hFlush stdout

    input' <- liftIO getLine

    let raw        = parseRawInput input'
    let normalized = normalizeInput raw
    let input'     = zipWith Input raw normalized
    input .= input'
    -- liftIO $ dumpInputs raw 

    let action = head input'
    let arg = if length input' > 1
            then Just . head . tail $ input'
            else Nothing
    let args = tail input'

    liftIO $ putStrLn ""
    processAction action arg args
    liftIO $ putStrLn ""

loadInternal :: GameLoop
loadInternal = do
    Data.setState
    game <- get
    -- liftIO $ exportGame game
    liftIO . putStrLn $ ""
    liftIO . putStrLn $ "Running against internal config"
    liftIO . putStrLn $ ""

loadExternal :: String -> GameLoop
loadExternal file = do
    -- if "load" `elem` args 
    game <- loadGame file
    liftIO . putStrLn $ ""
    -- liftIO $ pPrint game
    put $ fromJust game

importGame :: IO (Maybe Game)
importGame = do
    handle   <- openFile "json/in.json" ReadMode
    contents <- BL.hGetContents handle
    -- BL.putStr contents
    let game = decode contents :: Maybe Game
    -- print game 
    pure game

exportGame :: Game -> IO ()
exportGame state = do
    BL.writeFile "json/out.json" $ encodePretty state

dumpInputs :: [String] -> IO ()
dumpInputs = print . zip [0 ..]

loadGame :: (MonadState Game m, MonadIO m) => String -> m (Maybe Game)
loadGame file = runMaybeT $ do
    let path = "json/" ++ file ++ ".json"
    handle   <- liftIO $ openFile path ReadMode
    contents <- liftIO $ BL.hGetContents handle
    let gameExt = decode contents :: Maybe GameExt
    case gameExt of
        Nothing      -> hoistMaybe Nothing
        Just gameExt -> do
            game <- toGame gameExt
            hoistMaybe $ Just game
