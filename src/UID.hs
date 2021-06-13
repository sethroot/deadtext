{-# LANGUAGE FlexibleContexts #-}

module UID where

import           Control.Lens                   ( (%=)
                                                , use
                                                )
import           Control.Monad.State.Lazy       ( MonadState )
import           Types                          ( Game
                                                , HasUidGen(uidGen)
                                                )

genUid :: (MonadState Game m) => m Int
genUid = do
    uid <- use uidGen
    uidGen %= (+ 1)
    pure uid
