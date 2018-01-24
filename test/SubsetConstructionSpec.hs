module SubsetConstructionSpec
  ( spec
  ) where

import qualified Data.Set as Set
import SubsetConstruction
import Test.Hspec
import Types

spec :: Spec
spec =
  describe "The nfa2dfa function" $ do
    it "should create a dfa from nfa representing the regex \"a\"" $
      nfa2dfa
        NFA
        { nfaSigma = Set.singleton ((NFAState 0, Just 'a'), NFAState 1)
        , nfaStart = NFAState 0
        , nfaAcceptingStates = Set.singleton $ NFAState 1
        } `shouldBe`
      DFA
      { dfaSigma = Set.singleton ((DFAState 0, 'a'), DFAState 1)
      , dfaStart = DFAState 0
      , dfaAcceptingStates = Set.singleton $ DFAState 1
      }
    it "should create a dfa from nfa representing the regex \"ab\"" $
      nfa2dfa
        NFA
        { nfaSigma =
            Set.fromList
              [ ((NFAState 0, Just 'a'), NFAState 1)
              , ((NFAState 1, Nothing), NFAState 2)
              , ((NFAState 2, Just 'b'), NFAState 3)
              ]
        , nfaStart = NFAState 0
        , nfaAcceptingStates = Set.singleton $ NFAState 3
        } `shouldBe`
      DFA
      { dfaSigma =
          Set.fromList
            [((DFAState 0, 'a'), DFAState 1), ((DFAState 1, 'b'), DFAState 2)]
      , dfaStart = DFAState 0
      , dfaAcceptingStates = Set.singleton $ DFAState 2
      }
    it "should create a dfa from nfa representing the regex \"a|b\"" $
      nfa2dfa
        NFA
        { nfaSigma =
            Set.fromList
              [ ((NFAState 0, Nothing), NFAState 1)
              , ((NFAState 0, Nothing), NFAState 3)
              , ((NFAState 1, Just 'a'), NFAState 2)
              , ((NFAState 3, Just 'b'), NFAState 4)
              , ((NFAState 2, Nothing), NFAState 5)
              , ((NFAState 4, Nothing), NFAState 5)
              ]
        , nfaStart = NFAState 0
        , nfaAcceptingStates = Set.singleton $ NFAState 5
        } `shouldBe`
      DFA
      { dfaSigma =
          Set.fromList
            [((DFAState 0, 'a'), DFAState 1), ((DFAState 0, 'b'), DFAState 2)]
      , dfaStart = DFAState 0
      , dfaAcceptingStates = Set.fromList [DFAState 1, DFAState 2]
      }
    it "should create a dfa from nfa representing the regex \"a*\"" $
      nfa2dfa
        NFA
        { nfaSigma =
            Set.fromList
              [ ((NFAState 0, Nothing), NFAState 1)
              , ((NFAState 0, Nothing), NFAState 3)
              , ((NFAState 1, Just 'a'), NFAState 2)
              , ((NFAState 2, Nothing), NFAState 1)
              , ((NFAState 2, Nothing), NFAState 3)
              ]
        , nfaStart = NFAState 0
        , nfaAcceptingStates = Set.singleton $ NFAState 3
        } `shouldBe`
      DFA
      { dfaSigma =
          Set.fromList
            [((DFAState 0, 'a'), DFAState 1), ((DFAState 1, 'a'), DFAState 1)]
      , dfaStart = DFAState 0
      , dfaAcceptingStates = Set.fromList [DFAState 0, DFAState 1]
      }
    it "should create a dfa from nfa representing the regex \"(a|b)*\"" $
      nfa2dfa
        NFA
        { nfaSigma =
            Set.fromList
              [ ((NFAState 0, Nothing), NFAState 1)
              , ((NFAState 0, Nothing), NFAState 7)
              , ((NFAState 1, Nothing), NFAState 2)
              , ((NFAState 1, Nothing), NFAState 4)
              , ((NFAState 2, Just 'a'), NFAState 3)
              , ((NFAState 3, Nothing), NFAState 6)
              , ((NFAState 4, Just 'b'), NFAState 5)
              , ((NFAState 5, Nothing), NFAState 6)
              , ((NFAState 6, Nothing), NFAState 7)
              , ((NFAState 6, Nothing), NFAState 1)
              ]
        , nfaStart = NFAState 0
        , nfaAcceptingStates = Set.singleton $ NFAState 7
        } `shouldBe`
      DFA
      { dfaSigma =
          Set.fromList
            [ ((DFAState 0, 'a'), DFAState 1)
            , ((DFAState 0, 'b'), DFAState 2)
            , ((DFAState 1, 'a'), DFAState 1)
            , ((DFAState 1, 'b'), DFAState 2)
            , ((DFAState 2, 'a'), DFAState 1)
            , ((DFAState 2, 'b'), DFAState 2)
            ]
      , dfaStart = DFAState 0
      , dfaAcceptingStates = Set.fromList [DFAState 0, DFAState 1, DFAState 2]
      }
    it "should create a dfa from nfa representing the regex \"a|b*\"" $
      nfa2dfa
        NFA
        { nfaSigma =
            Set.fromList
              [ ((NFAState 0, Nothing), NFAState 1)
              , ((NFAState 0, Nothing), NFAState 3)
              , ((NFAState 1, Just 'a'), NFAState 2)
              , ((NFAState 2, Nothing), NFAState 7)
              , ((NFAState 3, Nothing), NFAState 4)
              , ((NFAState 3, Nothing), NFAState 6)
              , ((NFAState 4, Just 'b'), NFAState 5)
              , ((NFAState 5, Nothing), NFAState 4)
              , ((NFAState 5, Nothing), NFAState 6)
              , ((NFAState 6, Nothing), NFAState 7)
              ]
        , nfaStart = NFAState 0
        , nfaAcceptingStates = Set.singleton $ NFAState 7
        } `shouldBe`
      DFA
      { dfaSigma =
          Set.fromList
            [ ((DFAState 0, 'a'), DFAState 1)
            , ((DFAState 0, 'b'), DFAState 2)
            , ((DFAState 2, 'b'), DFAState 2)
            ]
      , dfaStart = DFAState 0
      , dfaAcceptingStates = Set.fromList [DFAState 0, DFAState 1, DFAState 2]
      }
    it "should create a dfa from nfa representing the regex \"a(c*|bd)*bd|b*e\"" $
      nfa2dfa
        NFA
        { nfaSigma =
            Set.fromList
              [ ((NFAState 0, Nothing), NFAState 1)
              , ((NFAState 0, Nothing), NFAState 19)
              , ((NFAState 1, Just 'a'), NFAState 2)
              , ((NFAState 2, Nothing), NFAState 3)
              , ((NFAState 3, Nothing), NFAState 4)
              , ((NFAState 3, Nothing), NFAState 14)
              , ((NFAState 4, Nothing), NFAState 5)
              , ((NFAState 4, Nothing), NFAState 9)
              , ((NFAState 5, Nothing), NFAState 6)
              , ((NFAState 5, Nothing), NFAState 8)
              , ((NFAState 6, Just 'c'), NFAState 7)
              , ((NFAState 7, Nothing), NFAState 6)
              , ((NFAState 7, Nothing), NFAState 8)
              , ((NFAState 8, Nothing), NFAState 13)
              , ((NFAState 9, Just 'b'), NFAState 10)
              , ((NFAState 10, Nothing), NFAState 11)
              , ((NFAState 11, Just 'd'), NFAState 12)
              , ((NFAState 12, Nothing), NFAState 13)
              , ((NFAState 13, Nothing), NFAState 4)
              , ((NFAState 13, Nothing), NFAState 14)
              , ((NFAState 14, Nothing), NFAState 15)
              , ((NFAState 15, Just 'b'), NFAState 16)
              , ((NFAState 16, Nothing), NFAState 17)
              , ((NFAState 17, Just 'd'), NFAState 18)
              , ((NFAState 18, Nothing), NFAState 25)
              , ((NFAState 19, Nothing), NFAState 20)
              , ((NFAState 19, Nothing), NFAState 22)
              , ((NFAState 20, Just 'b'), NFAState 21)
              , ((NFAState 21, Nothing), NFAState 20)
              , ((NFAState 21, Nothing), NFAState 22)
              , ((NFAState 22, Nothing), NFAState 23)
              , ((NFAState 23, Just 'e'), NFAState 24)
              , ((NFAState 24, Nothing), NFAState 25)
              ]
        , nfaStart = NFAState 0
        , nfaAcceptingStates = Set.singleton $ NFAState 25
        } `shouldBe`
      DFA
      { dfaSigma =
          Set.fromList
            [ ((DFAState 0, 'a'), DFAState 1)
            , ((DFAState 0, 'b'), DFAState 5)
            , ((DFAState 0, 'e'), DFAState 6)
            , ((DFAState 1, 'b'), DFAState 4)
            , ((DFAState 1, 'c'), DFAState 2)
            , ((DFAState 2, 'b'), DFAState 4)
            , ((DFAState 2, 'c'), DFAState 2)
            , ((DFAState 3, 'b'), DFAState 4)
            , ((DFAState 3, 'c'), DFAState 2)
            , ((DFAState 4, 'd'), DFAState 3)
            , ((DFAState 5, 'b'), DFAState 5)
            , ((DFAState 5, 'e'), DFAState 6)
            ]
      , dfaStart = DFAState 0
      , dfaAcceptingStates = Set.fromList [DFAState 3, DFAState 6]
      }
