module ThompsonsConstructionSpec (spec) where

import qualified Data.Set              as Set
import           Test.Hspec
import           ThompsonsConstruction
import           Types

spec :: Spec
spec =

  describe "The re2nfa function" $ do

    it "should create a nfa from the regex of \"a\"" $
      re2nfa (PrimitiveRE 'a') `shouldBe`
        NFA
          { nfaSigma = Set.singleton ((NFAState 0, Just 'a'), NFAState 1)
          , nfaStart = NFAState 0
          , nfaAcceptingStates = Set.singleton $ NFAState 1
          }

    it "should create a nfa from the regex of \"ab\"" $
      re2nfa (ConcatenatedRE (PrimitiveRE 'a') (PrimitiveRE 'b')) `shouldBe`
        NFA
          { nfaSigma = Set.fromList [ ((NFAState 0, Just 'a'), NFAState 1)
                                    , ((NFAState 1, Nothing ), NFAState 2)
                                    , ((NFAState 2, Just 'b'), NFAState 3)
                                    ]
          , nfaStart = NFAState 0
          , nfaAcceptingStates = Set.singleton $ NFAState 3
          }

    it "should create a nfa from the regex of \"a|b\"" $
      re2nfa (AlternativeRE (PrimitiveRE 'a') (PrimitiveRE 'b')) `shouldBe`
        NFA
          { nfaSigma = Set.fromList [ ((NFAState 0, Nothing ), NFAState 1)
                                    , ((NFAState 0, Nothing ), NFAState 3)
                                    , ((NFAState 1, Just 'a'), NFAState 2)
                                    , ((NFAState 3, Just 'b'), NFAState 4)
                                    , ((NFAState 2, Nothing ), NFAState 5)
                                    , ((NFAState 4, Nothing ), NFAState 5)
                                    ]
          , nfaStart = NFAState 0
          , nfaAcceptingStates = Set.singleton $ NFAState 5
          }

    it "should create a nfa from the regex of \"a*\"" $
      re2nfa (ClosureRE (PrimitiveRE 'a')) `shouldBe`
        NFA
          { nfaSigma = Set.fromList [ ((NFAState 0, Nothing ), NFAState 1)
                                    , ((NFAState 0, Nothing ), NFAState 3)
                                    , ((NFAState 1, Just 'a'), NFAState 2)
                                    , ((NFAState 2, Nothing ), NFAState 1)
                                    , ((NFAState 2, Nothing ), NFAState 3)
                                    ]
          , nfaStart = NFAState 0
          , nfaAcceptingStates = Set.singleton $ NFAState 3
          }

    it "should create a nfa from the regex of \"(a|b)*\"" $
      re2nfa (ClosureRE (AlternativeRE (PrimitiveRE 'a') (PrimitiveRE 'b')))
        `shouldBe`
        NFA
          { nfaSigma = Set.fromList [ ((NFAState 0, Nothing ), NFAState 1)
                                    , ((NFAState 0, Nothing ), NFAState 7)
                                    , ((NFAState 1, Nothing ), NFAState 2)
                                    , ((NFAState 1, Nothing ), NFAState 4)
                                    , ((NFAState 2, Just 'a'), NFAState 3)
                                    , ((NFAState 3, Nothing ), NFAState 6)
                                    , ((NFAState 4, Just 'b'), NFAState 5)
                                    , ((NFAState 5, Nothing ), NFAState 6)
                                    , ((NFAState 6, Nothing ), NFAState 7)
                                    , ((NFAState 6, Nothing ), NFAState 1)
                                    ]
          , nfaStart = NFAState 0
          , nfaAcceptingStates = Set.singleton $ NFAState 7
          }

    it "should create a nfa from the regex of \"a|b*\"" $
      re2nfa (AlternativeRE (PrimitiveRE 'a') (ClosureRE (PrimitiveRE 'b')))
        `shouldBe`
        NFA
          { nfaSigma = Set.fromList [ ((NFAState 0, Nothing ), NFAState 1)
                                    , ((NFAState 0, Nothing ), NFAState 3)
                                    , ((NFAState 1, Just 'a'), NFAState 2)
                                    , ((NFAState 2, Nothing ), NFAState 7)
                                    , ((NFAState 3, Nothing ), NFAState 4)
                                    , ((NFAState 3, Nothing ), NFAState 6)
                                    , ((NFAState 4, Just 'b'), NFAState 5)
                                    , ((NFAState 5, Nothing ), NFAState 4)
                                    , ((NFAState 5, Nothing ), NFAState 6)
                                    , ((NFAState 6, Nothing ), NFAState 7)
                                    ]
          , nfaStart = NFAState 0
          , nfaAcceptingStates = Set.singleton $ NFAState 7
          }

    it "should create a nfa from the regex of \"a(c*|bd)*bd|b*e\"" $
      re2nfa (AlternativeRE
            (ConcatenatedRE
              (PrimitiveRE 'a')
              (ConcatenatedRE
                (ClosureRE
                  (AlternativeRE
                    (ClosureRE
                      (PrimitiveRE 'c')
                    )
                    (ConcatenatedRE
                      (PrimitiveRE 'b')
                      (PrimitiveRE 'd')
                    )
                  )
                )
                (ConcatenatedRE
                  (PrimitiveRE 'b')
                  (PrimitiveRE 'd')
                )
              )
            )
            (ConcatenatedRE
              (ClosureRE
                (PrimitiveRE 'b')
              )
              (PrimitiveRE 'e')
            )
           )
        `shouldBe`
        NFA
          { nfaSigma = Set.fromList [ ((NFAState  0, Nothing ), NFAState  1)
                                    , ((NFAState  0, Nothing ), NFAState 19)
                                    , ((NFAState  1, Just 'a'), NFAState  2)
                                    , ((NFAState  2, Nothing ), NFAState  3)
                                    , ((NFAState  3, Nothing ), NFAState  4)
                                    , ((NFAState  3, Nothing ), NFAState 14)
                                    , ((NFAState  4, Nothing ), NFAState  5)
                                    , ((NFAState  4, Nothing ), NFAState  9)
                                    , ((NFAState  5, Nothing ), NFAState  6)
                                    , ((NFAState  5, Nothing ), NFAState  8)
                                    , ((NFAState  6, Just 'c'), NFAState  7)
                                    , ((NFAState  7, Nothing ), NFAState  6)
                                    , ((NFAState  7, Nothing ), NFAState  8)
                                    , ((NFAState  8, Nothing ), NFAState 13)
                                    , ((NFAState  9, Just 'b'), NFAState 10)
                                    , ((NFAState 10, Nothing ), NFAState 11)
                                    , ((NFAState 11, Just 'd'), NFAState 12)
                                    , ((NFAState 12, Nothing ), NFAState 13)
                                    , ((NFAState 13, Nothing ), NFAState  4)
                                    , ((NFAState 13, Nothing ), NFAState 14)
                                    , ((NFAState 14, Nothing ), NFAState 15)
                                    , ((NFAState 15, Just 'b'), NFAState 16)
                                    , ((NFAState 16, Nothing ), NFAState 17)
                                    , ((NFAState 17, Just 'd'), NFAState 18)
                                    , ((NFAState 18, Nothing ), NFAState 25)
                                    , ((NFAState 19, Nothing ), NFAState 20)
                                    , ((NFAState 19, Nothing ), NFAState 22)
                                    , ((NFAState 20, Just 'b'), NFAState 21)
                                    , ((NFAState 21, Nothing ), NFAState 20)
                                    , ((NFAState 21, Nothing ), NFAState 22)
                                    , ((NFAState 22, Nothing ), NFAState 23)
                                    , ((NFAState 23, Just 'e'), NFAState 24)
                                    , ((NFAState 24, Nothing ), NFAState 25)
                                    ]
          , nfaStart = NFAState 0
          , nfaAcceptingStates = Set.singleton $ NFAState 25
          }
