-- Sam Olds
-- CS 5965
-- Dominion


-- All of the logic/strategy a player might use when selecting an action to take,
-- how to take an action, a card to buy, or cleanup


module Player
(
  chooseDiscards,
  chooseDefense,
  chooseAction,
  doAction,
  doBuy,
  doClean,
) where
import qualified Data.List as DL
import qualified Control.Lens as L
import Cards


--------------------------------------------------------------------------------------
-- Strategic Preferences                                                            --
--------------------------------------------------------------------------------------
-- Card orderings for which card to put first in a list.
-- Used for deciding which cards to discard first
discardPreference a b = compare (relativeRank a) (relativeRank b) where
  relativeRank Estate     = 1
  relativeRank Duchy      = 2
  relativeRank Province   = 3
  relativeRank Curse      = 4
  relativeRank Militia    = 5
  relativeRank Remodel    = 6
  relativeRank Workshop   = 7
  relativeRank Woodcutter = 8
  relativeRank Cellar     = 9
  relativeRank Mine       = 10
  relativeRank Smithy     = 11
  relativeRank Market     = 12
  relativeRank Village    = 13
  relativeRank Moat       = 14
  relativeRank Copper     = 15
  relativeRank Silver     = 16
  relativeRank Gold       = 17


-- Used for deciding which action cards to use first
actionPreference a b = compare (relativeRank a) (relativeRank b) where
  relativeRank Village    = 1
  relativeRank Market     = 2
  relativeRank Smithy     = 3
  relativeRank Mine       = 4
  relativeRank Cellar     = 5
  relativeRank Moat       = 6
  relativeRank Woodcutter = 7
  relativeRank Workshop   = 8
  relativeRank Militia    = 9
  relativeRank Remodel    = 10


-- Used for deciding which cards to buy first
buyPreference a b = compare (relativeRank a) (relativeRank b) where
  relativeRank Province   = 1
  relativeRank Gold       = 2
  relativeRank Mine       = 3
  relativeRank Market     = 4
  relativeRank Smithy     = 5
  relativeRank Village    = 6
  relativeRank Remodel    = 7
  relativeRank Duchy      = 8
  relativeRank Silver     = 9
  relativeRank Militia    = 10
  relativeRank Workshop   = 11
  relativeRank Woodcutter = 12
  relativeRank Cellar     = 13
  relativeRank Moat       = 14
  relativeRank Estate     = 15
  relativeRank Copper     = 16
  relativeRank Curse      = 17


-- Used for deciding which cards to replace first
replacePreference a b = compare (relativeRank a) (relativeRank b) where
  relativeRank Curse      = 1
  relativeRank Workshop   = 2
  relativeRank Woodcutter = 3
  relativeRank Estate     = 4
  relativeRank Militia    = 5
  relativeRank Remodel    = 6
  relativeRank Copper     = 7
  relativeRank Silver     = 8
  relativeRank Cellar     = 9
  relativeRank Moat       = 10
  relativeRank Smithy     = 11
  relativeRank Village    = 12
  relativeRank Market     = 13
  relativeRank Mine       = 14
  relativeRank Duchy      = 15
  relativeRank Gold       = 16
  relativeRank Province   = 17


-- Used for deciding which cards to block with first
blockPreference a b = compare (relativeRank a) (relativeRank b) where
  relativeRank Moat = 1


-- Discards a specific amount of cards depending on the type of attack card used. Discards
-- cards according to a preference list
chooseDiscards :: Play -> State -> Maybe Defense
chooseDiscards play state
  | attackCard == Militia = Just (Discard (discardCards ((length cardsInHand) - 3) cardsInHand))
  | otherwise             = Nothing
  where getActionCards (Act cards) = cards
        attackCard = head (getActionCards play)
        cardsInHand = L.view hand state
        discardCards amt cards = take amt (DL.sortBy discardPreference cards)


-- Ordered preference on what type of block card to use against an attack
chooseDefense :: [Card] -> Play -> State -> Maybe Defense
chooseDefense blockCards play state
  | null blockCards = Nothing
  | otherwise       = Just (Block blockCard)
  where blockCard = head (DL.sortBy blockPreference blockCards)


-- Ordered preference on what type of action to take. If the user has a
-- Village action card in their hand, use it first. Then Market, Smithy,
-- Mine, Cellear, Woodcutter, Workshop, Remodel
chooseAction :: [Card] -> State -> Maybe Play
chooseAction actCards state
  | actionsLeft == 0            = Nothing
  | not (null preferredActions) = doAction (head preferredActions) state
  | otherwise                   = Nothing
  where actionsLeft      = L.view actions state
        preferredActions = DL.sortBy actionPreference actCards


-- Ordered preference on what type of card to buy. If the user has enough
-- money, buy a Province, otherwise Gold, otherwise Mine, Market, Smithy,
-- etc
doBuy :: State -> Maybe Play
doBuy state
  | buysLeft == 0 || coinsLeft == 0  = Nothing
  | otherwise                        = getBestBuy preferredBuys
  where coinsLeft  = L.view coins state
        buysLeft   = L.view buys state
        supplyLeft = L.view supply state
        preferredBuys = DL.sortBy buyPreference supplyLeft
        getBestBuy (card:cards) = if coinsLeft >= getPrice card then
                                     Just (Buy card)
                                  else if null cards then
                                     Nothing
                                  else
                                     getBestBuy cards


-- If the user doesn't have any cards left in their hand, do a cleanup
-- play, otherwise do a cleanup with the first card in their hand
doClean :: State -> Maybe Play
doClean state
  | null cardsInHand = Just (Clean [])
  | otherwise        = Just (Clean [head (DL.sortBy discardPreference cardsInHand)])
  where cardsInHand  = L.view hand state


--------------------------------------------------------------------------------------
-- How to actually take each specific action                                        --
--------------------------------------------------------------------------------------
-- Simply plays a generic Action card
doGenericAction :: Card -> (State -> Maybe Play)
doGenericAction card = (\state -> if (L.view actions state) > 0 then -- make sure there are actions
                                     case tryMoveCard card hand plays -- moves the card from the hand to the plays
                                                      (updateStateValueBy actions -- decrements the actions count by 1
                                                                          (negate 1)
                                                                          state)
                                     of   Nothing -> Nothing
                                          Just _  -> Just (Act [card])
                                  else Nothing)


-- Accepts a card, returns a function that accepts a state and modifies it in some way
doAction :: Card -> (State -> Maybe Play)

-- Tries to upgrade silver to gold, or copper to silver, or gold to gold?
doAction Mine       = (\state -> if (L.view actions state) > 0 then -- make sure there are actions
                                    case tryMoveCard Mine hand plays -- moves a Mine card from the hand to the plays
                                                     (updateStateValueBy actions -- decrements the actions count by 1
                                                                         (negate 1)
                                                                         state)
                                    of   Nothing -> Nothing -- If there was no mine card in the hand, action didn't work
                                         Just usedActionState ->
                                              let viewHand = L.view hand state
                                              in  case tryReplaceCard Silver Gold hand usedActionState of -- try replacing silver with gold
                                                  Just _  -> Just (Act [Mine, Silver, Gold])
                                                  Nothing -> case tryReplaceCard Copper Silver hand usedActionState of -- try replacing copper with silver
                                                             Just _  -> Just (Act [Mine, Copper, Silver])
                                                             Nothing -> case tryReplaceCard Gold Gold hand usedActionState of -- replace gold with gold
                                                                        Just _  -> Just (Act [Mine, Gold, Gold])
                                                                        Nothing -> Nothing
                                 else Nothing)

-- Tries to trade out all victory cards from the hand
doAction Cellar     = (\state -> if (L.view actions state) > 0 then -- make sure there are actions
                                    case tryMoveCard Cellar hand plays -- moves a Cellar card from the hand to the plays
                                                     (updateStateValueBy actions -- decrements the actions count by 1
                                                                         (negate 1)
                                                                         state)
                                    of   Nothing -> Nothing
                                         Just usedActionState ->
                                              let viewHand = L.view hand usedActionState 
                                                  victHand = getCardsFrom Victory viewHand
                                              in  Just (Act (Cellar : victHand))
                                 else Nothing)

-- Simply plays the Market Card
doAction Market     = doGenericAction Market

-- Tries to replace a card with a better card that costs no more than 2 than the price of the replaced card
doAction Remodel    = (\state -> if (L.view actions state) > 0 then -- make sure there are actions
                                    case tryMoveCard Remodel hand plays -- moves a Remodel card from the hand to the plays
                                                     (updateStateValueBy actions -- decrements the actions count by 1
                                                                         (negate 1)
                                                                         state)
                                    of   Nothing -> Nothing
                                         Just usedActionState ->
                                              let viewHand       = L.view hand usedActionState
                                                  viewSupply     = L.view supply state
                                                  cardToReplace  = head (DL.sortBy replacePreference viewHand)
                                                  replacePrice   = getPrice cardToReplace
                                                  getBestReplace (card:cards) = if (getPrice card) <= (replacePrice + 2) then
                                                                                   Just card
                                                                                else if null cards then
                                                                                   Nothing
                                                                                else
                                                                                   getBestReplace cards
                                                  preferredCards = DL.sortBy buyPreference viewSupply
                                              in  case getBestReplace preferredCards of
                                                  Nothing -> Nothing
                                                  Just cardToAdd -> Just (Act [Remodel, cardToReplace, cardToAdd])
                                 else Nothing)

-- Simply plays the Smithy Card
doAction Smithy     = doGenericAction Smithy

-- Simply plays the Village Card
doAction Village    = doGenericAction Village

-- Simply plays the Woodcutter Card
doAction Woodcutter = doGenericAction Woodcutter

-- TODO: Clean this up, it's a little too verbose
-- Tries to get a series of cards: Smithy, Village, Remodel, Woodcutter, Workshop, Cellar 
doAction Workshop   = (\state -> if (L.view actions state) > 0 then -- make sure there are actions
                                    case tryMoveCard Workshop hand plays -- moves a Workshop card from the hand to the plays
                                                     (updateStateValueBy actions -- decrements the actions count by 1
                                                                         (negate 1)
                                                                         state)
                                    of   Nothing -> Nothing -- If there was no workshop card in the hand, action didn't work
                                         Just usedActionState ->
                                              case tryMoveCard Smithy supply hand usedActionState of -- try getting a Smithy from the supply
                                              Just _  -> Just (Act [Workshop, Smithy])
                                              Nothing -> case tryMoveCard Village supply hand usedActionState of -- try getting a Village from the supply
                                                         Just _  -> Just (Act [Workshop, Village])
                                                         Nothing -> case tryMoveCard Remodel supply hand usedActionState of -- try getting a Remodel from the supply
                                                                    Just _  -> Just (Act [Workshop, Remodel])
                                                                    Nothing -> case tryMoveCard Woodcutter supply hand usedActionState of -- try getting a Woodcutter from the supply
                                                                               Just _  -> Just (Act [Workshop, Woodcutter])
                                                                               Nothing -> case tryMoveCard Workshop supply hand usedActionState of -- try getting a Workshop from the supply
                                                                                          Just _  -> Just (Act [Workshop, Workshop])
                                                                                          Nothing -> case tryMoveCard Cellar supply hand usedActionState of -- try getting a Cellar from the supply
                                                                                                     Just _  -> Just (Act [Workshop, Cellar])
                                                                                                     Nothing -> Nothing
                                 else Nothing)

-- Simply plays the Moat Card
doAction Moat       = doGenericAction Moat

-- Simply plays the Militia Card
doAction Militia    = doGenericAction Militia
