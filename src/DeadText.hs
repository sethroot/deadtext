module DeadText
    ( deadText
    ) where

import           Control.Lens                   ( (.=)
                                                , (^.)
                                                )
import           Control.Monad                  ( forever
                                                , void
                                                )
import           Control.Monad.IO.Class         ( liftIO )
import           Control.Monad.Trans.State.Lazy ( StateT(runStateT) )
import           Data.Aeson                     ( decode
                                                , encode
                                                )
import qualified Data.ByteString.Lazy          as BL
import           System.IO                      ( IOMode(ReadMode)
                                                , hClose
                                                , hFlush
                                                , openFile
                                                , stdout
                                                )

import           Action
import           Data                           ( initState
                                                , initWorld
                                                )
import           Parsing                        ( normalizeInput
                                                , parseRawInput
                                                )
import           Types                          ( Ext
                                                , GameLoop
                                                , HasInput(input)
                                                , HasNormal(normal)
                                                , Input(Input)
                                                )
import           Util                           ( )

deadText :: IO ()
deadText = void $ runStateT deadText' initState

deadText' :: GameLoop
deadText' = do
  -- liftIO importGame
  -- liftIO exportGame
    initWorld
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
        _           -> walkAction $ Just action

    liftIO $ putStrLn ""
    -- dumpGameState

importGame :: IO ()
importGame = do
    handle   <- openFile "json/game.json" ReadMode
    contents <- BL.hGetContents handle
    BL.putStr contents
    let a = decode contents :: Maybe Ext
    print a
    hClose handle

exportGame :: IO ()
exportGame = BL.writeFile "json/dump.json" $ encode initState
