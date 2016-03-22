-- Sam Olds
-- CS 5965
-- Dominion


-- This is the entry point for a dominion player


import qualified Control.Lens as L
import qualified Cards
import qualified Parse
import qualified Player
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
               Right notif -> putStrLn (respond notif)
  main -- Keep waiting for input



-- Responds with the correct responds depending on the notification type
respond :: Cards.Notification -> String
-- Make a play with the state, get state from notification
respond (Cards.Move state)               = case Play.play state of
                                                -- Print out move if successful
                                                Just newPlay -> show newPlay
                                                Nothing -> ""
-- Don't do anything
respond (Cards.Moved name play)          = ""
-- Either show a defense card or "take a hit"
respond (Cards.Attacked play name state) = case Play.defend play state of
                                                -- Print out move if successful
                                                Just defense -> show defense
                                                Nothing -> ""
-- Don't do anything
respond (Cards.Defended name defense)    = ""


-- Prints out a message to standard error
printError err = System.IO.hPrint System.IO.stderr err


--------------------------------------------------------------------------------------
-- TODO                                                                             --
--------------------------------------------------------------------------------------
-- Add unit tests!
-- Robustify parser (make it not procedural)
-- Make parser work better with being case insensitive
-- Get everything working without newlines being provided at the end of a notification by the server?
