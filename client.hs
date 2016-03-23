-- Sam Olds
-- CS 5965
-- Dominion


-- This is the entry point for a dominion player


import qualified Parse
import qualified Play
import qualified System.IO as SIO


--------------------------------------------------------------------------------------
-- Main                                                                             --
--------------------------------------------------------------------------------------
main :: IO ()
main = do
  -- Get user input
  gameState <- getLine

  -- Break on double line break
  if null gameState
     then return ()
     -- Parse the game
     else case Parse.parseState gameState of 
               Left err -> printError $ show err
               -- Parse was successful, get a response
               Right notif -> case Play.respond notif of
                                   Nothing -> putStr ""
                                   Just response -> putStr (show response)
  SIO.hFlush SIO.stdout -- flush the output buffer
  main -- Keep waiting for input



-- Prints out a message to standard error
printError err = SIO.hPrint SIO.stderr err


--------------------------------------------------------------------------------------
-- TODO                                                                             --
--------------------------------------------------------------------------------------
-- Add unit tests!
-- Robustify parser (make it not procedural)
-- Make parser work better with being case insensitive
-- Get everything working without newlines being provided at the end of a notification by the server?
