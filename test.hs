-- Sam Olds
-- CS 5965
-- Dominion


-- Unit Tests!


{-# LANGUAGE TemplateHaskell #-}
import Test.QuickCheck
import qualified Control.Lens as L
import Cards
import qualified Play
import qualified Parse


main :: IO ()
main = let stringNotif = "(move ((players barry wally bart jay) (supply Silver Silver Silver Silver Gold Gold Gold Gold) " ++
                         "       (trash) (actions 1) (buys 1) (coins 0) (deck) (hand Copper Copper Copper Copper Copper Mine Mine Mine Mine Mine) (plays) (discards)))"
           badString   = "(move ((trash blurpy blurps) (actions 1) (buys 1) (coins 0) (deck) (hand Copper Copper Copper Copper Copper Mine Mine Mine Mine Mine) (plays) (discards)))"
           realNotif   = Move (State ["barry", "wally", "bart", "jay"] [Silver, Silver, Silver, Silver, Gold, Gold, Gold, Gold] [] 1 1 0 [] [Copper, Copper, Copper, Copper, Copper, Mine, Mine, Mine, Mine, Mine] [] [])
           badNotif    = Move (State ["bad", "wally", "bart", "jay"] [Silver, Silver, Silver, Silver, Gold, Gold, Gold, Gold] [] 1 1 0 [] [Copper, Copper, Copper, Copper, Copper, Mine, Mine, Mine, Mine, Mine] [] [])
           realState   = State ["barry", "wally", "bart", "jay"] [Silver, Silver, Silver, Silver, Gold, Gold, Gold, Gold] [] 1 1 0 [] [Copper, Copper, Copper, Copper, Copper, Mine, Mine, Mine, Mine, Mine] [] []
           goodPlay    = Act [Mine, Copper, Silver]
           badPlay     = Clean []
       in  do
             quickCheck (testGoodParse stringNotif realNotif)
             quickCheck (testParseError badString)
             quickCheck (testBadParse stringNotif badNotif)
             quickCheck (testGoodPlay realState goodPlay)
             quickCheck (testBadPlay realState badPlay)






testGoodParse stringNotif realNotif = case (Parse.parseState stringNotif) of
                                           Left err -> False
                                           Right notif -> notif == realNotif

testBadParse stringNotif realNotif = case (Parse.parseState stringNotif) of
                                          Left err -> False
                                          Right notif -> notif /= realNotif

testParseError stringBadNotif = case (Parse.parseState stringBadNotif) of
                                          Left err -> True
                                          Right notif -> False

testGoodPlay state goodPlay = case (Play.play state) of
                                   Nothing -> False
                                   Just play -> play == goodPlay

testBadPlay state badPlay = case (Play.play state) of
                                 Nothing -> False
                                 Just play -> play /= badPlay




-- FOR REFERENCE
{-

( (players   eddy   )    ( supply Copper ) (trash duchy ) (actions 0 ) (buys 0 ) (coins 0 ) (      deck      )  
  (hand ) (plays ) (discards estate estate estate estate estate estate estate estate estate estate estate estate estate gold gold  estate estate ) )

((players Barry Wally Bart Jay) (supply Silver Silver Silver Silver Gold Gold Gold Gold) (trash) (actions 1) (buys 1) (coins 0) (deck) (hand Copper Copper Copper Copper Copper Mine Mine Mine Mine Mine) (plays) (discards))

(moved Barry (act Mine Copper Silver))

(moved Barry (add Copper))


(move ((players Barry Wally Bart Jay) (supply Silver Silver Silver Silver Gold Gold Gold Gold) (trash) (actions 1) (buys 1) (coins 0) (deck) (hand Silver Silver Gold Gold Mine Village Market) (plays) (discards)))

-}
