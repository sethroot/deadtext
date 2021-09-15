{-# LANGUAGE FlexibleContexts #-}

module Util where

import           Control.Monad.IO.Class         ( liftIO )
import           Control.Monad.State.Lazy       ( get )
import           Data.Char                      ( toLower )
import qualified Data.Map.Strict               as M
import           Text.Pretty.Simple             ( pPrint )
import           Types                          ( Game
                                                , GameLoop
                                                )

invert :: (Ord v) => M.Map k [v] -> M.Map v [k]
invert m = M.fromListWith (++) pairs
    where pairs = [ (v, [k]) | (k, vs) <- M.toList m, v <- vs ]

fromBool :: Bool -> a -> Maybe a
fromBool False = const Nothing
fromBool True  = Just

(?) :: Bool -> a -> a -> a
(?) True  x _ = x
(?) False _ y = y

maybeToEither :: Maybe a -> Either () a
maybeToEither Nothing  = Left ()
maybeToEither (Just a) = Right a

lowEq :: (Functor f, Eq (f Char)) => f Char -> f Char -> Bool
lowEq a b = (toLower <$> a) == (toLower <$> b)

debugGameState :: GameLoop
debugGameState = do
    game <- get
    liftIO $ printGame game

printGame :: Game -> IO ()
printGame g = do
    putStrLn ""
    pPrint g
    putStrLn ""
