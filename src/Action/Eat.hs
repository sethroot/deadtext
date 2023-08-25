module Action.Eat (eatAction) where
import Types

eatAction :: (Monad m) => [Input] -> m ()
eatAction _ = return ()
