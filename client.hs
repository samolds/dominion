-- Sam Olds
-- CS 5965
-- Dominion


-- This is the entry point for a dominion player


import qualified Parse
import qualified Play
import qualified System.IO


--------------------------------------------------------------------------------------
-- Main                                                                             --
--------------------------------------------------------------------------------------
main :: IO ()
main = do
  -- Get user input
  gameState <- getLine
  -- printError $ show "DEBUG-- "
  -- printError gameState

  -- Break on double line break
  if null gameState
     then return ()
     -- Parse the game
     else case Parse.parseState gameState of 
               Left err -> printError $ show err
               -- Parse was successful, get a response
               Right notif -> putStrLn (Play.respond notif)
  main -- Keep waiting for input



-- Prints out a message to standard error
printError err = System.IO.hPrint System.IO.stderr err


--------------------------------------------------------------------------------------
-- TODO                                                                             --
--------------------------------------------------------------------------------------
-- Add unit tests!
-- Robustify parser (make it not procedural)
-- Make parser work better with being case insensitive
-- Get everything working without newlines being provided at the end of a notification by the server?
