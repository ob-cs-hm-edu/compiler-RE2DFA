module HopcroftSpec (spec) where

import qualified Data.Set   as Set
import           Hopcroft
import           Test.Hspec
import           Types

spec :: Spec
spec =

  describe "The dfa2min function" $ do

    it "should create a minimal dfa from the dfa of \"a\"" $
      dfa2min
        DFA
          { dfaSigma = Set.singleton ((DFAState 0, 'a'), DFAState 1)
          , dfaStart = DFAState 0
          , dfaAcceptingStates = Set.singleton $ DFAState 1
          }
      `shouldBe`
        DFA
          { dfaSigma = Set.singleton ((DFAState 0, 'a'), DFAState 1)
          , dfaStart = DFAState 0
          , dfaAcceptingStates = Set.singleton $ DFAState 1
          }

    it "should create a minimal dfa from the dfa of \"ab\"" $
      dfa2min
        DFA
          { dfaSigma = Set.fromList [ ((DFAState 0, 'a'), DFAState 1)
                                    , ((DFAState 1, 'b'), DFAState 2)
                                    ]
          , dfaStart = DFAState 0
          , dfaAcceptingStates = Set.singleton $ DFAState 2
          }
      `shouldBe`
        DFA
          { dfaSigma = Set.fromList [ ((DFAState 0, 'a'), DFAState 1)
                                    , ((DFAState 1, 'b'), DFAState 2)
                                    ]
          , dfaStart = DFAState 0
          , dfaAcceptingStates = Set.singleton $ DFAState 2
          }

    it "should create a minimal dfa from the dfa of \"a|b\"" $
      dfa2min
        DFA
          { dfaSigma = Set.fromList [ ((DFAState 0, 'a'), DFAState 1)
                                    , ((DFAState 0, 'b'), DFAState 2)
                                    ]
          , dfaStart = DFAState 0
          , dfaAcceptingStates = Set.fromList [ DFAState 1, DFAState 2 ]
          }
      `shouldBe`
        DFA
          { dfaSigma = Set.fromList [ ((DFAState 0, 'a'), DFAState 1)
                                    , ((DFAState 0, 'b'), DFAState 1)
                                    ]
          , dfaStart = DFAState 0
          , dfaAcceptingStates = Set.singleton (DFAState 1)
          }

    it "should create a minimal dfa from the dfa of \"a*\"" $
      dfa2min
        DFA
          { dfaSigma = Set.fromList [ ((DFAState 0, 'a'), DFAState 1)
                                    , ((DFAState 1, 'a'), DFAState 1)
                                    ]
          , dfaStart = DFAState 0
          , dfaAcceptingStates = Set.fromList [ DFAState 0, DFAState 1 ]
          }
      `shouldBe`
        DFA
          { dfaSigma = Set.fromList [ ((DFAState 0, 'a'), DFAState 0)
                                    ]
          , dfaStart = DFAState 0
          , dfaAcceptingStates = Set.singleton (DFAState 0)
          }

    it "should create a minimal dfa from the dfa of \"(a|b)*\"" $
      dfa2min
        DFA
          { dfaSigma = Set.fromList [ ((DFAState 0, 'a'), DFAState 1)
                                    , ((DFAState 0, 'b'), DFAState 2)
                                    , ((DFAState 1, 'a'), DFAState 1)
                                    , ((DFAState 1, 'b'), DFAState 2)
                                    , ((DFAState 2, 'a'), DFAState 1)
                                    , ((DFAState 2, 'b'), DFAState 2)
                                    ]
          , dfaStart = DFAState 0
          , dfaAcceptingStates = Set.fromList [ DFAState 0
                                              , DFAState 1
                                              , DFAState 2
                                              ]
          }
      `shouldBe`
        DFA
          { dfaSigma = Set.fromList [ ((DFAState 0, 'a'), DFAState 0)
                                    , ((DFAState 0, 'b'), DFAState 0)
                                    ]
          , dfaStart = DFAState 0
          , dfaAcceptingStates = Set.singleton (DFAState 0)
          }

    it "should create a minimal dfa from the dfa of \"a|b*\"" $
      dfa2min
        DFA
          { dfaSigma = Set.fromList [ ((DFAState 0, 'a'), DFAState 1)
                                    , ((DFAState 0, 'b'), DFAState 2)
                                    , ((DFAState 2, 'b'), DFAState 2)
                                    ]
          , dfaStart = DFAState 0
          , dfaAcceptingStates = Set.fromList [ DFAState 0
                                              , DFAState 1
                                              , DFAState 2
                                              ]
          }
      `shouldBe`
        DFA
          { dfaSigma = Set.fromList [ ((DFAState 0, 'a'), DFAState 1)
                                    , ((DFAState 0, 'b'), DFAState 2)
                                    , ((DFAState 2, 'b'), DFAState 2)
                                    ]
          , dfaStart = DFAState 0
          , dfaAcceptingStates = Set.fromList [ DFAState 0
                                              , DFAState 1
                                              , DFAState 2
                                              ]
          }

    it "should create a minimal dfa from the dfa of \"fee|fie\"" $
      dfa2min
        DFA
          { dfaSigma = Set.fromList [ ((DFAState 0, 'f'), DFAState 1)
                                    , ((DFAState 1, 'e'), DFAState 2)
                                    , ((DFAState 1, 'i'), DFAState 4)
                                    , ((DFAState 2, 'e'), DFAState 3)
                                    , ((DFAState 4, 'e'), DFAState 5)
                                    ]
          , dfaStart = DFAState 0
          , dfaAcceptingStates = Set.fromList [ DFAState 3
                                              , DFAState 5
                                              ]
          }
      `shouldBe`
        DFA
          { dfaSigma = Set.fromList [ ((DFAState 0, 'f'), DFAState 1)
                                    , ((DFAState 1, 'e'), DFAState 2)
                                    , ((DFAState 1, 'i'), DFAState 2)
                                    , ((DFAState 2, 'e'), DFAState 3)
                                    ]
          , dfaStart = DFAState 0
          , dfaAcceptingStates = Set.fromList [ DFAState 3
                                              ]
          }

    it "should create a minimal dfa from the dfa of \"a(b|c*)\"" $
      dfa2min
        DFA
          { dfaSigma = Set.fromList [ ((DFAState 0, 'a'), DFAState 1)
                                    , ((DFAState 1, 'b'), DFAState 2)
                                    , ((DFAState 1, 'c'), DFAState 3)
                                    , ((DFAState 2, 'b'), DFAState 2)
                                    , ((DFAState 2, 'c'), DFAState 3)
                                    , ((DFAState 3, 'b'), DFAState 2)
                                    , ((DFAState 3, 'c'), DFAState 3)
                                    ]
          , dfaStart = DFAState 0
          , dfaAcceptingStates = Set.fromList [ DFAState 1
                                              , DFAState 2
                                              , DFAState 3
                                              ]
          }
      `shouldBe`
        DFA
          { dfaSigma = Set.fromList [ ((DFAState 0, 'a'), DFAState 1)
                                    , ((DFAState 1, 'b'), DFAState 1)
                                    , ((DFAState 1, 'c'), DFAState 1)
                                    ]
          , dfaStart = DFAState 0
          , dfaAcceptingStates = Set.fromList [ DFAState 1
                                              ]
          }
