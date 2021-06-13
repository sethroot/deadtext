{-# LANGUAGE FlexibleContexts #-}

module Util where

import qualified Data.Map.Strict               as M

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
