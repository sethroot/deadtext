import DeadText
import Action.Give

main :: IO ()
-- main = putStrLn "Test suite not yet implemented"
-- main = deadText
main = do
  parsed <- parseInput "give map to angela"
  action <- tokenize parsed
  out <- exec action
  pure ()