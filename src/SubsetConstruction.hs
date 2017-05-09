module SubsetConstruction
  ( nfa2dfa
  ) where

import qualified Data.Map   as Map
import           Data.Maybe (fromJust, isNothing)
import qualified Data.Set   as Set
import           Types

nfa2dfa :: NFA -> DFA
nfa2dfa nfa = dfaFromQandT nfa $ subsetConstruction nfa

type Q = Set.Set (Set.Set NFAState)
type T = Set.Set ((Set.Set NFAState,Char), Set.Set NFAState)

dfaFromQandT :: NFA -> (Q,T) -> DFA
dfaFromQandT nfa (q,t) =
  let nfaAndDfaStates = Map.fromList
                      $ map (\(i, nfastates) -> (nfastates, DFAState i))
                      $ zip [0..]
                      $ Set.toList q
      dfaSigmaFromT = Set.map (\((fromStates, c), toStates) ->
                               ((fromJust $ Map.lookup fromStates
                                                       nfaAndDfaStates
                                ,c
                                )
                               , fromJust $ Map.lookup toStates
                                                       nfaAndDfaStates
                               )
                              ) t
  in DFA
      { dfaSigma = dfaSigmaFromT
      , dfaStart = fromJust $ Map.lookup (epsilonClosure (Set.singleton
                                                          $ nfaStart nfa) nfa)
                                         nfaAndDfaStates
      , dfaAcceptingStates = Set.fromList
                           $ Map.elems
                           $ (\s -> Map.filterWithKey
                                  (\k _ -> k `Set.member` s) nfaAndDfaStates)
                           $ Set.filter
                                ( not
                                . Set.null
                                . Set.intersection (nfaAcceptingStates nfa)) q
      }

subsetConstruction :: NFA -> (Q,T)
subsetConstruction nfa =
  let q0 = epsilonClosure (Set.singleton $ nfaStart nfa) nfa
  in subsetConstruction' (Set.singleton q0) nfa (Set.singleton q0, Set.empty)

subsetConstruction' :: Set.Set (Set.Set NFAState) -> NFA -> (Q,T) -> (Q,T)
subsetConstruction' worklist nfa qt@(qs, ts) =
  if Set.null worklist
  then qt
  else let q = Set.elemAt 0 worklist
           worklist' = Set.deleteAt 0 worklist
           ts' = Set.fromList
               $ filter (not . Set.null . snd)
               $ map (\c -> ((q,c), (`epsilonClosure` nfa) $ delta q nfa c))
               $ nfaBigSigma nfa
           ts'' = ts `Set.union` ts'
           tsNotInQs = Set.map snd ts' Set.\\ qs
           qs' = qs `Set.union` tsNotInQs
           worklist'' = worklist' `Set.union` tsNotInQs
       in subsetConstruction' worklist'' nfa (qs', ts'')

delta :: Set.Set NFAState -> NFA -> Char-> Set.Set NFAState
delta states nfa c =
  let args = Set.map (\s -> (s,Just c)) states
  in Set.map snd
     $ Set.filter ((`Set.member` args) . fst)
     $ nfaSigma nfa

epsilonClosure :: Set.Set NFAState -> NFA -> Set.Set NFAState
epsilonClosure reachedStates nfa =
  let newStates = Set.union reachedStates
                $ Set.map snd
                $ Set.filter (\((from, with), _) ->
                            isNothing with && from  `elem` reachedStates)
                $ nfaSigma nfa
  in if newStates == reachedStates
     then newStates
     else epsilonClosure newStates nfa
