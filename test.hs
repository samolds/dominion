-- Sam Olds
-- CS 5965
-- Dominion


-- Unit Tests!


{-# LANGUAGE TemplateHaskell #-}
import Test.QuickCheck
import Cards
import qualified Play
import qualified Parse


--------------------------------------------------------------------------------------
-- Main entry point for all unit tests                                              --
--------------------------------------------------------------------------------------
main :: IO ()
main = do
         testFullTurn
         testParse
         testRespond
         testPlay
         ---- TODO: Write much many more tests!


--------------------------------------------------------------------------------------
-- End to End System Tests                                                          --
--------------------------------------------------------------------------------------
testFullTurn = let moveStateString1 = "(move ((players 1-sam 0-player-v3-soldier) (supply copper copper copper copper copper copper copper copper copper copper copper copper copper copper copper copper copper copper " ++
                                      "copper copper copper copper copper copper copper copper copper copper copper copper copper copper copper copper copper copper copper copper copper copper copper copper copper copper " ++
                                      "copper copper silver silver silver silver silver silver silver silver silver silver silver silver silver silver silver silver silver silver silver silver silver silver silver silver " ++
                                      "silver silver silver silver silver silver silver silver silver silver silver silver silver silver silver silver gold gold gold gold gold gold gold gold gold gold gold gold gold gold " ++
                                      "gold gold gold gold gold gold gold gold gold gold gold gold gold gold gold gold estate estate estate estate estate estate estate estate duchy duchy duchy duchy duchy duchy duchy " ++
                                      "duchy province province province province province province province province mine mine mine mine mine mine mine mine mine mine cellar cellar cellar cellar cellar cellar cellar cellar " ++
                                      "cellar cellar market market market market market market market market market market remodel remodel remodel remodel remodel remodel remodel remodel remodel remodel smithy smithy " ++
                                      "smithy smithy smithy smithy smithy smithy smithy smithy village village village village village village village village village village woodcutter woodcutter woodcutter woodcutter " ++
                                      "woodcutter woodcutter woodcutter woodcutter woodcutter woodcutter workshop workshop workshop workshop workshop workshop workshop workshop workshop workshop militia militia militia " ++
                                      "militia militia militia militia militia militia militia moat moat moat moat moat moat moat moat moat) (trash) (actions 1) (buys 1) (coins 0) (deck copper copper copper estate estate) " ++
                                      "(hand copper estate copper copper copper) (plays) (discards)))"
                   moveStateString2 = "(move ((players 1-sam 0-player-v3-soldier) (supply copper copper copper copper copper copper copper copper copper copper copper copper copper copper copper copper copper copper " ++
                                      "copper copper copper copper copper copper copper copper copper copper copper copper copper copper copper copper copper copper copper copper copper copper copper copper copper copper " ++
                                      "copper copper silver silver silver silver silver silver silver silver silver silver silver silver silver silver silver silver silver silver silver silver silver silver silver silver " ++
                                      "silver silver silver silver silver silver silver silver silver silver silver silver silver silver silver silver gold gold gold gold gold gold gold gold gold gold gold gold gold gold " ++
                                      "gold gold gold gold gold gold gold gold gold gold gold gold gold gold gold gold estate estate estate estate estate estate estate estate duchy duchy duchy duchy duchy duchy duchy " ++
                                      "duchy province province province province province province province province mine mine mine mine mine mine mine mine mine mine cellar cellar cellar cellar cellar cellar cellar cellar " ++
                                      "cellar cellar market market market market market market market market market market remodel remodel remodel remodel remodel remodel remodel remodel remodel remodel smithy smithy " ++
                                      "smithy smithy smithy smithy smithy smithy smithy smithy village village village village village village village village village village woodcutter woodcutter woodcutter woodcutter " ++
                                      "woodcutter woodcutter woodcutter woodcutter woodcutter woodcutter workshop workshop workshop workshop workshop workshop workshop workshop workshop workshop militia militia militia " ++
                                      "militia militia militia militia militia militia militia moat moat moat moat moat moat moat moat moat) (trash) (actions 1) (buys 1) (coins 4) (deck copper copper copper estate estate) " ++
                                      "(hand estate) (plays copper copper copper copper) (discards)))"
                   moveStateString3 = "(move ((players 1-sam 0-player-v3-soldier) (supply copper copper copper copper copper copper copper copper copper copper copper copper copper copper copper copper copper copper " ++
                                      "copper copper copper copper copper copper copper copper copper copper copper copper copper copper copper copper copper copper copper copper copper copper copper copper copper copper " ++
                                      "copper copper silver silver silver silver silver silver silver silver silver silver silver silver silver silver silver silver silver silver silver silver silver silver silver silver " ++
                                      "silver silver silver silver silver silver silver silver silver silver silver silver silver silver silver silver gold gold gold gold gold gold gold gold gold gold gold gold gold gold " ++
                                      "gold gold gold gold gold gold gold gold gold gold gold gold gold gold gold gold estate estate estate estate estate estate estate estate duchy duchy duchy duchy duchy duchy duchy " ++
                                      "duchy province province province province province province province province mine mine mine mine mine mine mine mine mine mine cellar cellar cellar cellar cellar cellar cellar cellar " ++
                                      "cellar cellar market market market market market market market market market market remodel remodel remodel remodel remodel remodel remodel remodel remodel remodel smithy smithy " ++
                                      "smithy smithy smithy smithy smithy smithy smithy smithy village village village village village village village village village village woodcutter woodcutter woodcutter woodcutter " ++
                                      "woodcutter woodcutter woodcutter woodcutter woodcutter woodcutter workshop workshop workshop workshop workshop workshop workshop workshop workshop workshop militia militia militia " ++
                                      "militia militia militia militia militia militia militia moat moat moat moat moat moat moat moat moat) (trash) (actions 1) (buys 1) (coins 0) (deck copper copper copper estate estate) " ++
                                      "(hand village mine copper copper copper) (plays) (discards)))"
                   response1        = PlayMade (Add Copper)
                   response2        = PlayMade (Buy Smithy)
                   response3        = PlayMade (Act [Village])
               in  do
                     quickCheck (testProperResponse moveStateString1 response1)
                     quickCheck (testProperResponse moveStateString2 response2)
                     quickCheck (testProperResponse moveStateString3 response3)

testProperResponse string response = case (Parse.parseState string) of
                                          Left err -> False
                                          Right notif -> case (Play.respond notif) of
                                                              Nothing -> False
                                                              Just playResponse -> playResponse == response

--------------------------------------------------------------------------------------
-- Parser                                                                           --
--------------------------------------------------------------------------------------
testParse = let moveStringNotif   = "(move ((players barry wally bart jay) (supply Silver Silver Silver Silver Gold Gold Gold Gold) " ++
                                    "(trash) (actions 1) (buys 1) (coins 0) (deck) (hand Copper Copper Copper Copper Copper Mine Mine Mine Mine Mine) (plays) (discards)))"
                movedStringNotif  = "(moved barry (add silver))"
                attackStringNotif = "(attacked (act militia) wally ((players barry wally) (supply) (trash) (actions 0) (buys 0) (coins 0) (deck) (hand) (plays) (discards)))"
                defendStringNotif = "(defended wally (moat))"
                badStringNotif    = "(move ((trash blurpy blurps) (actions 1) (buys 1) (coins 0) (deck) (hand Copper Copper Copper Copper Copper Mine Mine Mine Mine Mine) (plays) (discards)))"

                moveNotif    = Move (State ["barry", "wally", "bart", "jay"] [Silver, Silver, Silver, Silver, Gold, Gold, Gold, Gold] [] 1 1 0 [] [Copper, Copper, Copper, Copper, Copper, Mine, Mine, Mine, Mine, Mine] [] [])
                badMoveNotif = Move (State ["bad", "wally", "bart", "jay"] [Silver, Silver, Silver, Silver, Gold, Gold, Gold, Gold] [] 1 1 0 [] [Copper, Copper, Copper, Copper, Copper, Mine, Mine, Mine, Mine, Mine] [] [])
                movedNotif   = Moved "barry" (Add Silver)
                attackNotif  = Attacked (Act [Militia]) "wally" (State ["barry", "wally"] [] [] 0 0 0 [] [] [] [])
                defendNotif  = Defended "wally" (Block Moat)

            in  do
                  quickCheck (testGoodParse moveStringNotif moveNotif)
                  quickCheck (testGoodParse movedStringNotif movedNotif)
                  quickCheck (testGoodParse attackStringNotif attackNotif)
                  quickCheck (testGoodParse defendStringNotif defendNotif)
                  quickCheck (testParseError badStringNotif)
                  quickCheck (testBadParse moveStringNotif badMoveNotif)

testGoodParse stringNotif realNotif = case (Parse.parseState stringNotif) of
                                           Left err -> False
                                           Right notif -> notif == realNotif

testBadParse stringNotif realNotif = case (Parse.parseState stringNotif) of
                                          Left err -> False
                                          Right notif -> notif /= realNotif

testParseError stringBadNotif = case (Parse.parseState stringBadNotif) of
                                          Left err -> True
                                          Right notif -> False


--------------------------------------------------------------------------------------
-- Respond                                                                          --
--------------------------------------------------------------------------------------
testRespond = let state1      = State ["barry", "wally"] [] [] 0 0 0 [] [Copper, Estate, Estate, Copper, Copper] [] []
                  state2      = State ["barry", "wally"] [] [] 1 1 0 [] [Copper, Estate, Estate, Moat, Copper] [] []
                  state3      = State ["barry", "wally"] [] [] 0 0 0 [] [Estate, Copper, Mine, Militia, Copper] [] []
                  state4      = State ["barry", "wally"] [] [] 1 1 0 [] [Copper, Copper, Copper, Copper, Copper] [] []
                  state5      = State ["barry", "wally"] [Silver, Mine, Duchy, Copper, Mine, Moat, Province] [] 1 1 5 [] [] [] []
                  state6      = State ["barry", "wally"] [Silver, Mine, Duchy, Copper, Mine, Moat, Province] [] 0 0 0 [] [] [] []
                  attackPlay  = (Act [Militia])
              in  do
                    quickCheck (case Play.defend attackPlay state1 of
                                     Nothing -> False
                                     Just defense -> (Cards.Discard [Estate, Estate]) == defense)
                    quickCheck (case Play.defend attackPlay state2 of
                                     Nothing -> False
                                     Just defense -> (Cards.Block Moat) == defense)
                    quickCheck (case Play.defend attackPlay state3 of
                                     Nothing -> False
                                     Just defense -> (Cards.Discard [Estate, Militia]) == defense)
                    quickCheck (case Play.play state2 of
                                     Nothing -> False
                                     Just playMade -> (Act [Moat]) == playMade)
                    quickCheck (case Play.play state4 of
                                     Nothing -> False
                                     Just playMade -> (Add Copper) == playMade)
                    quickCheck (case Play.play state6 of
                                     Nothing -> False
                                     Just playMade -> (Clean []) == playMade)



--------------------------------------------------------------------------------------
-- Play                                                                             --
--------------------------------------------------------------------------------------
testPlay = let realState = State ["barry", "wally", "bart", "jay"] [Silver, Silver, Silver, Silver, Gold, Gold, Gold, Gold] [] 1 1 0 [] [Copper, Copper, Copper, Copper, Copper, Mine, Mine, Mine, Mine, Mine] [] []
               goodPlay  = Act [Mine, Copper, Silver]
               badPlay   = Clean []
           in  do
                 quickCheck (testGoodPlay realState goodPlay)
                 quickCheck (testBadPlay realState badPlay)

testGoodPlay state goodPlay = case (Play.play state) of
                                   Nothing -> False
                                   Just play -> play == goodPlay

testBadPlay state badPlay = case (Play.play state) of
                                 Nothing -> False
                                 Just play -> play /= badPlay




--------------------------------------------------------------------------------------
-- JUST FOR REFERENCE                                                               --
--------------------------------------------------------------------------------------
{-

( (players   eddy   )    ( supply Copper ) (trash duchy ) (actions 0 ) (buys 0 ) (coins 0 ) (      deck      )  
  (hand ) (plays ) (discards estate estate estate estate estate estate estate estate estate estate estate estate estate gold gold  estate estate ) )

((players Barry Wally Bart Jay) (supply Silver Silver Silver Silver Gold Gold Gold Gold) (trash) (actions 1) (buys 1) (coins 0) (deck) (hand Copper Copper Copper Copper Copper Mine Mine Mine Mine Mine) (plays) (discards))

(moved Barry (act Mine Copper Silver))

(moved Barry (add Copper))


(move ((players Barry Wally Bart Jay) (supply Silver Silver Silver Silver Gold Gold Gold Gold) (trash) (actions 1) (buys 1) (coins 0) (deck) (hand Silver Silver Gold Gold Mine Village Market) (plays) (discards)))

-}
