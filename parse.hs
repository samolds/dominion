-- Sam Olds
-- CS 5965
-- Dominion


-- Attempts to Parse any string, using parser combinators, into a Notification
-- Good Resources:
--                http://book.realworldhaskell.org/read/using-parsec.html
--                https://hackage.haskell.org/package/parsec-3.1.9/docs/Text-ParserCombinators-Parsec-Prim.html#v:skipMany
--                https://wiki.haskell.org/Parsing_expressions_and_statements
--                https://wiki.haskell.org/Parsing_a_simple_imperative_language


module Parse
(
  parseState
) where
import qualified Text.ParserCombinators.Parsec.Number as Number
import Text.ParserCombinators.Parsec
import qualified Cards
import Data.Char(toUpper, toLower)



--------------------------------------------------------------------------------------
-- Entry point to Parser                                                            --
--------------------------------------------------------------------------------------
-- Entry Point to the whole Parser Combinator
-- This whole thing is essentially a context free grammar
-- NOTE: Converts the whole string to a lowercase before parsing it out
--       Only matters because it means it will lowercase the players names.
--       TODO: Does this matter?
parseState :: String -> Either ParseError Cards.Notification
parseState input = parse notification "(stdin)" (caseInsensitiveString input)

-- Match the string 's', accepting either lowercase or uppercase form of each character 
caseInsensitiveString :: String -> String
caseInsensitiveString s = [toLower c | c <- s]


-- TODO: ewwww -> VERY procedural...
-- I feel like I'm using this at a 6th grade comprehension level
notification :: GenParser Char st Cards.Notification
notification = do
                 spaces
                 char '('
                 content <- notificationCollection
                 char ')'
                 eof
                 return content

notificationCollection :: GenParser Char st Cards.Notification
notificationCollection = try (string "moved"    >> notificationMoved)
                     <|> try (string "move"     >> notificationMove)
                     <|> try (string "attacked" >> notificationAttacked)
                     <|> try (string "defended" >> notificationDefended)

notificationMoved :: GenParser Char st Cards.Notification
notificationMoved = do
                      spaces
                      name <- (many (noneOf " ()\r\n"))
                      spaces
                      content <- play
                      spaces
                      return (Cards.Moved name content)

notificationMove :: GenParser Char st Cards.Notification
notificationMove = do
                     spaces
                     content <- gameState
                     spaces
                     return (Cards.Move content)

-- (attacked (act militia) name state)
notificationAttacked :: GenParser Char st Cards.Notification
notificationAttacked = do
                         spaces
                         content <- play
                         spaces
                         name <- (many (noneOf " ()\r\n"))
                         spaces
                         stateContent <- gameState
                         spaces
                         return (Cards.Attacked content name stateContent)

-- (defended name defense)
notificationDefended :: GenParser Char st Cards.Notification
notificationDefended = do
                         spaces
                         name <- (many (noneOf " ()\r\n"))
                         spaces
                         content <- defense
                         spaces
                         return (Cards.Defended name content)

play :: GenParser Char st Cards.Play
play = do
         spaces
         char '('
         content <- playCollection
         char ')'
         spaces
         return content

playCollection :: GenParser Char st Cards.Play
playCollection = try (string "act"   >> playAct)
             <|> try (string "add"   >> playAdd)
             <|> try (string "buy"   >> playBuy)
             <|> try (string "clean" >> playClean)

playAct :: GenParser Char st Cards.Play
playAct = do
            spaces
            content <- sepBy card (char ' ' <* spaces)
            return (Cards.Act content)

playAdd :: GenParser Char st Cards.Play
playAdd = do
            spaces
            content <- card
            return (Cards.Add content)

playBuy :: GenParser Char st Cards.Play
playBuy = do
            spaces
            content <- card
            return (Cards.Buy content)

playClean :: GenParser Char st Cards.Play
playClean = do
              spaces
              content <- sepBy card (char ' ' <* spaces)
              return (Cards.Clean content)


defense :: GenParser Char st Cards.Defense
defense = do
         spaces
         char '('
         content <- defenseCollection
         char ')'
         spaces
         return content

defenseCollection :: GenParser Char st Cards.Defense
defenseCollection = try (string "discard" >> defenseDiscard)
                <|> defenseBlock 

defenseBlock :: GenParser Char st Cards.Defense
defenseBlock = do
                 spaces
                 content <- card
                 return (Cards.Block content)

defenseDiscard :: GenParser Char st Cards.Defense
defenseDiscard = do
                   spaces
                   content <- sepBy card (char ' ' <* spaces)
                   return (Cards.Discard content)



gameState :: GenParser Char st Cards.State
gameState = do
               spaces
               char '('
               a <- playerCollection
               b <- cardCollection
               c <- cardCollection
               d <- numCollection
               e <- numCollection
               f <- numCollection
               g <- cardCollection
               h <- cardCollection
               i <- cardCollection
               j <- cardCollection
               char ')' <?> "parens wrap the state"
               spaces
               return (Cards.State a b c d e f g h i j)


playerCollection :: GenParser Char st [String]
playerCollection = do
                      spaces <* char '(' <* spaces
                      string "players" <* spaces
                      content <- sepBy (many (noneOf " ()\r\n")) (char ' ')
                      spaces <* string ")" <?> "parens wrap the statefield"
                      spaces
                      return content


cardCollection :: GenParser Char st [Cards.Card]
cardCollection = do
                    spaces <* char '(' <* spaces
                    cardCollectionType <* spaces
                    content <- sepBy card (char ' ' <* spaces)
                    spaces <* string ")" <?> "parens wrap the statefield"
                    spaces
                    return content


numCollection :: GenParser Char st Int
numCollection = do
                   spaces <* char '(' <* spaces
                   numCollectionType <* spaces
                   content <- Number.decimal
                   spaces <* string ")" <?> "parens wrap the statefield"
                   spaces
                   return content


cardCollectionType :: GenParser Char st String
cardCollectionType = try (string "supply")
                 <|> try (string "trash")
                 <|> try (string "deck")
                 <|> try (string "hand")
                 <|> try (string "plays")
                 <|> try (string "discards")
                 <?> "a valid collection type"


numCollectionType :: GenParser Char st String
numCollectionType = try (string "actions")
                <|> try (string "buys")
                <|> try (string "coins")
                <?> "a valid collection type"


-- TODO: Get this to handle text, insensitive to case, like a grownup...
--       This is so embarrasing
card :: GenParser Char st Cards.Card
card = try (string "province"   >> return Cards.Province)
   <|> try (string "duchy"      >> return Cards.Duchy)
   <|> try (string "estate"     >> return Cards.Estate)
   <|> try (string "curse"      >> return Cards.Curse)
   <|> try (string "gold"       >> return Cards.Gold)
   <|> try (string "silver"     >> return Cards.Silver)
   <|> try (string "copper"     >> return Cards.Copper)

   <|> try (string "mine"       >> return Cards.Mine)
   <|> try (string "cellar"     >> return Cards.Cellar) 
   <|> try (string "market"     >> return Cards.Market)
   <|> try (string "remodel"    >> return Cards.Remodel)
   <|> try (string "smithy"     >> return Cards.Smithy)
   <|> try (string "village"    >> return Cards.Village)
   <|> try (string "woodcutter" >> return Cards.Woodcutter)
   <|> try (string "workshop"   >> return Cards.Workshop)
   <|> try (string "moat"       >> return Cards.Moat)
   <|> try (string "militia"    >> return Cards.Militia)

   <?> "valid card"
