{-# LANGUAGE FlexibleContexts #-}

module DeadText
    ( deadText
    ) where

import           Action
import           Control.Error
import           Control.Lens                   ( (.=)
                                                , (^.)
                                                )
import           Control.Monad                  ( forever
                                                , void
                                                )
import           Control.Monad.IO.Class         ( liftIO )
import           Control.Monad.State.Lazy       ( MonadIO
                                                , MonadState
                                                )
import           Control.Monad.Trans.Maybe
import           Control.Monad.Trans.State.Lazy ( StateT(runStateT)
                                                , get
                                                , put
                                                )
import qualified Data
import           Data.Aeson                     ( decode )
import           Data.Aeson.Encode.Pretty       ( encodePretty )
import qualified Data.ByteString.Lazy          as BL
import           Data.Maybe
import           Load                           ( GameExt
                                                , toGame
                                                )
import           Parsing                        ( normalizeInput
                                                , parseRawInput
                                                )
import           System.Environment
import           System.IO                      ( IOMode(ReadMode)
                                                , hClose
                                                , hFlush
                                                , openFile
                                                , stdout
                                                )
import           Text.Pretty.Simple
import           Types                          ( Ext
                                                , Game
                                                , GameLoop
                                                , HasInput(input)
                                                , HasNormal(normal)
                                                , Input(Input)
                                                , Loc
                                                )
import           Util                           ( )

deadText :: IO ()
deadText = void $ runStateT deadText' Data.initState

deadText' :: GameLoop
deadText' = do
    args <- liftIO getArgs
    liftIO $ print args
    if "debug" `elem` args
        then do
            Data.initWorld
            game <- get
            -- liftIO $ exportGame game
            pure ()
        else do
            game <- loadGame
            -- liftIO $ pPrint game
            put $ fromJust game
    lookAction []
    forever execGameLoop
    pure ()

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

    case action ^. normal of
        "drop"      -> dropAction arg
        "give"      -> giveAction args
        "help"      -> helpAction
        "inv"       -> invAction
        "inventory" -> invAction
        "kill"      -> killAction arg
        "leave"     -> dropAction arg
        "look"      -> lookAction args
        "open"      -> openAction arg
        "pickup"    -> pickupAction arg
        "take"      -> pickupAction arg
        "talk"      -> talkAction arg
        "walk"      -> walkAction arg
        "debug"     -> debugGameState
        _           -> walkAction $ Just action

    liftIO $ putStrLn ""

importGame :: IO (Maybe Game)
importGame = do
    handle   <- openFile "json/in.json" ReadMode
    contents <- BL.hGetContents handle
    -- BL.putStr contents
    let game = decode contents :: Maybe Game
    -- hClose handle
    -- print game 
    pure game

exportGame :: Game -> IO ()
exportGame state = do
    BL.writeFile "json/out.json" $ encodePretty state

debugGameState :: GameLoop
debugGameState = do
    game <- get
    liftIO $ printGame game

printGame :: Game -> IO ()
printGame g = do
    putStrLn ""
    pPrint g
    putStrLn ""

dumpInputs :: [String] -> IO ()
dumpInputs = print . zip [0 ..]

loadGame :: (MonadState Game m, MonadIO m) => m (Maybe Game)
loadGame = runMaybeT $ do
    handle   <- liftIO $ openFile "json/game.json" ReadMode
    contents <- liftIO $ BL.hGetContents handle
    let gameExt = decode contents :: Maybe GameExt
    case gameExt of
        Nothing      -> hoistMaybe Nothing
        Just gameExt -> do
            game <- toGame gameExt
            hoistMaybe $ Just game
