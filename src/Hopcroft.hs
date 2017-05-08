module Hopcroft where

import qualified Data.Set as Set
import           GHC.Exts (groupWith)
import           Types

dfa2min :: DFA -> DFA
dfa2min dfa =
  let t = calcT dfa (initialT dfa) Set.empty
  in mkDFA dfa t

type T = Set.Set (Set.Set DFAState)
type P = T

initialT :: DFA -> T
initialT dfa =
  Set.filter (not . Set.null)
  $ Set.fromList
      [ dfaAcceptingStates dfa
      , dfaStates dfa Set.\\ dfaAcceptingStates dfa
      ]

calcT :: DFA -> T -> P -> T
calcT dfa t p =
  if t == p
  then t
  else let p' = t
           t'' = foldl (\t' p'' -> t' `Set.union` split dfa t p'') Set.empty t
       in calcT dfa t'' p'

split :: DFA -> T -> Set.Set DFAState -> Set.Set (Set.Set DFAState)
split dfa t p =
  let chars = dfaBigSigma dfa
      findSetInT :: Set.Set DFAState -> Set.Set DFAState
      findSetInT s =
        if null s
          then s
          else Set.elemAt 0 $ Set.filter (not . null . Set.intersection s) t
      statesAndResults c =
        map (Set.fromList . map fst)
        $ groupWith snd
        $ Set.toList
        $ Set.map (\s -> (s, findSetInT $ applySigmaToChar dfa s c)) p
      statesAndResultsForAllWhichSplits =
         filter ((>1) . length) $ map statesAndResults chars
  in if null statesAndResultsForAllWhichSplits
     then Set.singleton p
     else Set.fromList $ head statesAndResultsForAllWhichSplits

-- emptySet if no transition is found
applySigmaToChar :: DFA -> DFAState -> Char -> Set.Set DFAState
applySigmaToChar dfa state char =
  Set.map snd
  $ Set.filter (\((from, c), _) -> from == state && c == char)
  $ dfaSigma dfa

mkDFA :: DFA -> T -> DFA
mkDFA dfa t =
  let oldAndNewStates = zip (Set.toList t) $ map DFAState [0..]
      newStateFromOld s = snd $ head
        $ filter (\(states,_) -> s  `Set.member` states) oldAndNewStates
  in DFA
      { dfaSigma =
          Set.map (\((from, char), to) ->
            ((newStateFromOld from, char), newStateFromOld to)
          ) $ dfaSigma dfa
      , dfaStart =
          snd $ head $ filter ((dfaStart dfa `elem`) . fst) oldAndNewStates
      , dfaAcceptingStates = Set.fromList
          $ map snd
          $ filter (not . null . Set.intersection (dfaAcceptingStates dfa)  . fst)
                   oldAndNewStates
      }
