-- Sam Olds
-- CS 5965
-- Dominion


-- Backtracking process for attempting to play a player's turn. Attempts
-- to do some actions during the action phase, and if that fails it tries
-- to do some buys during the buy phase, otherwise it does a cleanup

module Play
( 
  play,
  defend,
) where
import qualified Control.Lens as L
import qualified Player
import qualified Cards



--------------------------------------------------------------------------------------
-- Baisc Turn Logic - Tries Action, Buy, Cleanup                                    --
--------------------------------------------------------------------------------------
-- Trys to do any possible actions, followed by any possible buys, followed by cleanup
play :: Cards.State -> Maybe Cards.Play
play state = case tryActions state of -- Tries to do actions
             Just notif -> Just notif -- Successful Action!
             Nothing -> case tryBuys state of -- Tries to do buys
                        Just notif -> Just notif -- Successful Buy!
                        Nothing -> case tryCleanups state of -- Tries to cleanup
                                   Just notif -> Just notif -- Succesful Clean!
                                   Nothing -> Nothing


-- If the player has any defense cards, use them to block the attack, otherwise
-- call the player's discard logic
defend :: Cards.Play -> Cards.State -> Maybe Cards.Defense
defend play state
  | length blocks > 0 = Player.chooseDefense blocks play state
  | otherwise         = Player.chooseDiscards play state
  where blocks = Cards.getCardsFrom Cards.Defend (L.view Cards.hand state)


--------------------------------------------------------------------------------------
-- Specifics of trying each phase                                                   --
--------------------------------------------------------------------------------------
-- Gets all of the action cards from the user's hand and lets the player decide how to
-- try (if they can)
tryActions :: Cards.State -> Maybe Cards.Play
tryActions state
  | (L.view Cards.actions state) > 0 && length actions > 0 = Player.chooseAction actions state
  | otherwise                                              = Nothing
  where actions = Cards.getCardsFrom Cards.Action (L.view Cards.hand state)


-- While the user still has unplayed coins in their hand add the coins, otherwise try
-- let the user try and buy something if they have money and buy actions left
tryBuys :: Cards.State -> Maybe Cards.Play
tryBuys state
  | length coinsInHand > 0                                          = Just (Cards.Add (head coinsInHand))
  | (L.view Cards.coins state) > 0 && (L.view Cards.buys state) > 0 = Player.doBuy state
  | otherwise                                                       = Nothing
  where coinsInHand = (Cards.getCardsFrom Cards.Treasure (L.view Cards.hand state))


-- Let the user decide how to do the cleanup phase
tryCleanups :: Cards.State -> Maybe Cards.Play
tryCleanups state = Player.doClean state
