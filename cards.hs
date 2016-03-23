-- Sam Olds
-- CS 5965
-- Dominion


-- This is all of the data record definitions for types used in Dominion
-- as well as a few collection manipulation helper functions.
-- Useful reference for all Dominion cards:
--    http://dominionstrategy.com/all-cards


-- Makes generation of getters and setters for state record
-- magically simple and hidden
{-# LANGUAGE TemplateHaskell, RankNTypes #-}
-- TODO: Use Lenses more explicitly (to learn), so that
--       this is not necessary.


module Cards
( 
  -- NOTE: `(..)` necessary to export the data constructor along with the type
  Card(..),
  CardType(..),
  ActionType(..),
  Defense(..),
  Notification(..),
  Response(..),
  Play(..),
  State(..),

  cardType,
  actionType,
  getPrice,
  getValue,
  getCardsFrom,

  players,  -- Lens getter/setter
  supply,   -- Lens getter/setter
  trash,    -- Lens getter/setter
  actions,  -- Lens getter/setter
  buys,     -- Lens getter/setter
  coins,    -- Lens getter/setter
  deck,     -- Lens getter/setter
  hand,     -- Lens getter/setter
  plays,    -- Lens getter/setter
  discards, -- Lens getter/setter

  tryBuyCard,
  simplePrint,
  tryMoveCard,
  tryReplaceCard,
  updateStateValueBy,
) where
import qualified Data.List as List
import qualified Control.Lens as L


--------------------------------------------------------------------------------------
-- Data Records                                                                     --
--------------------------------------------------------------------------------------
-- All Cards as "Enums"
data Card = Province | Duchy | Estate | Curse |
            Gold | Silver | Copper |
            Mine | Cellar | Market | Remodel | Smithy | Village | Woodcutter | Workshop | Moat | Militia
            deriving (Eq, Read, Bounded, Enum)
data CardType     = Victory | Treasure | Action deriving (Eq, Ord, Read, Bounded, Enum)
data ActionType   = General | Attack | Defend deriving (Eq, Ord, Read, Bounded, Enum)
data Notification = Move State | Moved String Play | Attacked Play String State | Defended String Defense deriving (Eq, Read)
data Response     = PlayMade Play | DefenseMade Defense deriving (Eq, Read)
data Play         = Act [Card] | Add Card | Buy Card | Clean [Card] deriving (Eq, Read)
data Defense      = Block Card | Discard [Card] deriving (Eq, Read)

-- Information necessary for a turn
data State = State {_players  :: [String], -- in order, current first
                    _supply   :: [Card],
                    _trash    :: [Card], -- in order, top to bottom

                    _actions  :: Int, -- actions remaining in turn
                    _buys     :: Int, -- buys remaining in turn
                    _coins    :: Int, -- coins available for buys

                    _deck     :: [Card], -- not in draw order
                    _hand     :: [Card],
                    _plays    :: [Card],
                    _discards :: [Card]} deriving (Eq, Read)
-- Makes getters and setters for each field (named without the underscores)
L.makeLenses ''State -- Requires the {-# LANGUAGE TemplateHaskell #-} GHC extension


--------------------------------------------------------------------------------------
-- Helper methods available to each of the cards                                    --
--------------------------------------------------------------------------------------
-- Each card's type
cardType :: Card -> CardType
cardType Province = Victory
cardType Duchy    = Victory
cardType Estate   = Victory
cardType Curse    = Victory
cardType Gold     = Treasure
cardType Silver   = Treasure
cardType Copper   = Treasure
cardType _        = Action

-- Each action card's type
actionType :: Card -> ActionType
actionType Militia  = Attack
actionType Moat     = Defend
actionType _        = General

-- How much each cards costs to buy
getPrice :: Card -> Int
getPrice Province   = 8
getPrice Duchy      = 5
getPrice Estate     = 2
getPrice Curse      = 0

getPrice Gold       = 6
getPrice Silver     = 3
getPrice Copper     = 0

getPrice Mine       = 5
getPrice Cellar     = 2
getPrice Market     = 5
getPrice Remodel    = 4
getPrice Smithy     = 4
getPrice Village    = 3
getPrice Woodcutter = 3
getPrice Workshop   = 3
getPrice Moat       = 2
getPrice Militia    = 4

-- How much each card is worth
getValue :: Card -> Int
getValue Province   = 6
getValue Duchy      = 3
getValue Estate     = 1
getValue Curse      = -1

getValue Gold       = 3
getValue Silver     = 2
getValue Copper     = 1

-- Used to overload the getCardsFrom function to accept multiple card types
class GetCardsFrom card where -- Just a Typeclass
  getCardsFrom :: card -> [Card] -> [Card]

-- Returns a list of cards from a collection that are the same
instance GetCardsFrom Card where
  getCardsFrom card set = filter (\x -> x == card) set

-- Returns a list of cards from a collection that are the same type
instance GetCardsFrom CardType where
  getCardsFrom card set = filter (\x -> (cardType x) == card) set

-- Returns a list of cards from a collection that are the same action type
instance GetCardsFrom ActionType where
  getCardsFrom card set = filter (\x -> (actionType x) == card) set

--------------------------------------------------------------------------------------
-- Creating custom "to string" methods for each data type                           --
--------------------------------------------------------------------------------------
instance Show Card where
  show Province    = "province"
  show Duchy       = "duchy"
  show Estate      = "estate"
  show Curse       = "curse"

  show Gold        = "gold"
  show Silver      = "silver"
  show Copper      = "copper"

  show Mine        = "mine"
  show Cellar      = "cellar"
  show Market      = "market"
  show Remodel     = "remodel"
  show Smithy      = "smithy"
  show Village     = "village"
  show Woodcutter  = "woodcutter"
  show Workshop    = "workshop"
  show Moat        = "moat"
  show Militia     = "militia"
 
-- How to print each card type
instance Show CardType where
  show Victory     = "victory"
  show Treasure    = "treasure"
  show Action      = "action"

-- How to print a notification
instance Show Notification where
  show (Move state)               = "(move "  ++ show state ++ ")"
  show (Moved name play)          = "(moved " ++ name ++ " " ++ show play ++ ")"
  show (Attacked play name state) = "(attacked " ++ show play ++ " " ++ name ++ " " ++ show state ++ ")"
  show (Defended name defense)    = "(defended " ++ name ++ " " ++ show defense ++ ")"

-- How to print a response
instance Show Response where
  show (PlayMade play)       = show play
  show (DefenseMade defense) = show defense

-- How to print a play
instance Show Play where
  show (Act cards)   = "(act"  ++ simplePrint cards ++ ")"
  show (Add card)    = "(add " ++ show card         ++ ")"
  show (Buy card)    = "(buy " ++ show card         ++ ")"
  show (Clean cards) = if null cards 
                          then "(clean)"
                          else "(clean" ++ simplePrint cards ++ ")"

-- How to print a defense
instance Show Defense where
  show (Block card)    = "(" ++ show card ++ ")"
  show (Discard cards) = if null cards 
                          then "(discard)"
                          else "(discard" ++ simplePrint cards ++ ")"

-- How to print the state
instance Show State where
  show state = "(" ++
               "(players " ++ List.intercalate " " (L.view players state) ++ ") " ++
               "(supply"   ++ simplePrint (L.view supply state)           ++ ") " ++
               "(trash"    ++ simplePrint (L.view trash state)            ++ ") " ++
               "(actions " ++ show (L.view actions state)                 ++ ") " ++
               "(buys "    ++ show (L.view buys state)                    ++ ") " ++
               "(coins "   ++ show (L.view coins state)                   ++ ") " ++
               "(deck"     ++ simplePrint (L.view deck state)             ++ ") " ++
               "(hand"     ++ simplePrint (L.view hand state)             ++ ") " ++
               "(plays"    ++ simplePrint (L.view plays state)            ++ ") " ++
               "(discards" ++ simplePrint (L.view discards state)         ++ ")"  ++
               ")"


--------------------------------------------------------------------------------------
-- Helper Methods for manipulation collections of cards or the state in general     --
--------------------------------------------------------------------------------------
-- Displays a list of cards as space separated
simplePrint :: [Card] -> String
simplePrint list = foldl (++) "" (map (" " ++) (map show list))
-- Could just do `List.intercalate " " (map show list)` instead


-- spend coins to buy card from supply and add it to deck
tryBuyCard :: Card -> State -> Maybe State
tryBuyCard card state = if getPrice card <= (L.view coins state) && -- make sure the card is affordable
                           (L.view buys state) > 1                  -- make sure there are enough buy actions left
                           then let buyState = updateStateValueBy coins -- subtract price of card from coins left
                                                                  (negate (getPrice card))
                                                                  (updateStateValueBy buys -- decrement buy actions
                                                                                      (negate 1)
                                                                                      state)
                                in  tryMoveCard card supply deck buyState
                        else Nothing


-- move card from one collection to another
tryMoveCard :: Card -> (L.Lens' State [Card]) -> (L.Lens' State [Card]) -> State -> Maybe State
tryMoveCard card getColl setColl state = if elem card (L.view getColl state) -- make sure the card exists in the "from" collection
                                         then let updatedState = L.set getColl
                                                                       (List.delete card (L.view getColl state))
                                                                       state -- remove card from the first collection
                                              in  Just (L.set setColl
                                                              (card : (L.view setColl updatedState))
                                                              updatedState)  -- add card to second collection
                                         else Nothing


-- replace card in a collection with another card from `supply` AND add replaced card to `trash`
-- used by the `Mine` and the `Remodel`
tryReplaceCard :: Card -> Card -> (L.Lens' State [Card]) -> State -> Maybe State
tryReplaceCard rCard aCard coll state = if elem aCard (L.view supply state) && -- make sure the card to add exists in the `supply` collection
                                           elem rCard (L.view coll state)      -- make sure the card to replace exists in the coll
                                        then let updatedSuppl = L.set supply
                                                                      (List.delete aCard (L.view supply state))
                                                                      state    -- remove card to add from the `supply`
                                                 updatedTrash = L.set trash
                                                                      (rCard : (L.view trash updatedSuppl))
                                                                      updatedSuppl -- add card to replace to `trash`
                                                 updatedState = L.set coll
                                                                      (List.delete rCard (L.view coll updatedTrash))
                                                                      updatedTrash -- remove card to replace from coll
                                             in  Just (L.set coll
                                                             (aCard : (L.view coll updatedState))
                                                             updatedState)     -- add card to to add to coll
                                        else Nothing


-- updates the actions, buys, or coins state by the specified amount
updateStateValueBy :: (L.Lens' State Int) -> Int -> State -> State
updateStateValueBy coll amount state = L.set coll ((L.view coll state) + amount) state
