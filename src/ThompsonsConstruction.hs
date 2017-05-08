module ThompsonsConstruction where

import qualified Data.Set as Set
import           Types

re2nfa :: RE -> NFA
re2nfa = re2nfa' 0

re2nfa' :: Integer -> RE -> NFA

re2nfa' n (PrimitiveRE c) = NFA
  { nfaSigma = Set.singleton ((NFAState n, Just c), NFAState (n+1))
  , nfaStart = NFAState n
  , nfaAcceptingStates = Set.singleton (NFAState (n+1))
  }

re2nfa' n (ConcatenatedRE re1 re2) =
  let nfa1 = re2nfa' n re1
      n' = highestStateNumber nfa1
      nfa2 = re2nfa' (n'+1) re2
  in NFA
      { nfaSigma = nfaSigma nfa1 `Set.union` nfaSigma nfa2
                   `Set.union` Set.map (\s -> ((s,Nothing), nfaStart nfa2))
                                        (nfaAcceptingStates nfa1)
      , nfaStart = nfaStart nfa1
      , nfaAcceptingStates = nfaAcceptingStates nfa2
      }

re2nfa' n (ClosureRE re) =
  let nfa = re2nfa' (n+1) re
      state0 = NFAState n
      state1 = nfaStart nfa
      stateEnd = NFAState (1 + highestStateNumber nfa)
  in NFA
      { nfaSigma = nfaSigma nfa
                   `Set.union`
                   Set.map (\s -> ((s,Nothing), stateEnd))
                           (nfaAcceptingStates nfa)
                   `Set.union`
                   Set.map (\s -> ((s,Nothing), state1))
                           (nfaAcceptingStates nfa)
                   `Set.union`
                   Set.fromList [ ((state0, Nothing), state1)
                                , ((state0, Nothing), stateEnd)
                                ]
      , nfaStart = state0
      , nfaAcceptingStates = Set.singleton stateEnd
      }

re2nfa' n (AlternativeRE re1 re2) =
  let state0 = NFAState n
      nfa1 = re2nfa' (n+1) re1
      n' = highestStateNumber nfa1
      nfa2 = re2nfa' (n'+1) re2
      stateEnd = NFAState (1 + highestStateNumber nfa2)
  in NFA
      { nfaSigma = nfaSigma nfa1 `Set.union` nfaSigma nfa2
          `Set.union` Set.fromList
                        [ ((state0, Nothing), nfaStart nfa1)
                        , ((state0, Nothing), nfaStart nfa2)
                        ]
          `Set.union` Set.map (\s -> ((s,Nothing), stateEnd))
                              (           nfaAcceptingStates nfa1
                              `Set.union` nfaAcceptingStates nfa2)
      , nfaStart = NFAState n
      , nfaAcceptingStates = Set.singleton stateEnd
      }

highestStateNumber :: NFA -> Integer
highestStateNumber = maximum . Set.map nfaStateNumber . nfaStates
