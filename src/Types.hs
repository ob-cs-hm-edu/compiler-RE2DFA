module Types
  ( RE(..)
  , NFAState(..)
  , NFA(..)
  , nfaStates
  , nfaBigSigma
  , DFAState(..)
  , DFA(..)
  , dfaStates
  , dfaBigSigma
  ) where

import Data.List (intercalate)
import Data.Maybe (catMaybes)
import qualified Data.Set as Set

data RE
  = PrimitiveRE Char
  | ConcatenatedRE RE
                   RE
  | AlternativeRE RE
                  RE
  | ClosureRE RE
  deriving (Eq)

instance Show RE where
  show (PrimitiveRE x) = [x]
  show (ConcatenatedRE re1 re2) = "(" ++ show re1 ++ show re2 ++ ")"
  show (AlternativeRE re1 re2) = "(" ++ show re1 ++ "|" ++ show re2 ++ ")"
  show (ClosureRE re) = show re ++ "*"

newtype NFAState = NFAState
  { nfaStateNumber :: Integer
  } deriving (Eq, Ord)

instance Show NFAState where
  show (NFAState i) = "n" ++ show i

data NFA = NFA
    -- nfaStates       :: Set.Set NFAState -- wird berechnet
    -- nfaBigSigma     :: [Char]           -- wird berechnet
  { nfaSigma :: Set.Set ((NFAState, Maybe Char), NFAState)
  , nfaStart :: NFAState
  , nfaAcceptingStates :: Set.Set NFAState
  } deriving (Eq, Ord)

nfaStates :: NFA -> Set.Set NFAState
nfaStates nfa =
  Set.singleton (nfaStart nfa) `Set.union` nfaAcceptingStates nfa `Set.union`
  Set.unions
    (map (\((from, _), to) -> Set.fromList [from, to]) $
     Set.toList $ nfaSigma nfa)

nfaBigSigma :: NFA -> String
nfaBigSigma = catMaybes . Set.toList . Set.map (snd . fst) . nfaSigma

instance Show NFA
  -- Ausgabe in Datei speichern und mit [dot](http://www.graphviz.org/)
  -- umwandeln, z.B. `dot -Tpng -o nfa.png nfa.dot`
                                                    where
  show (NFA sigma start acceptingStates) =
    "digraph NFA {\n" ++
    "  rankdir=LR;\n" ++
    "  node [shape = point, color=white, fontcolor=white]; start;\n" ++
    "  node [shape = doublecircle, color=black, fontcolor=black];\n  " ++
    intercalate ";\n" (map show $ Set.toList acceptingStates) ++
    ";\n" ++
    "  node [shape = circle];\n" ++
    "  start -> " ++
    show start ++
    ";\n  " ++
    intercalate
      "\n  "
      (map
         (\((from, with), to) ->
            show from ++
            " -> " ++
            show to ++
            case with of
              Just c -> "[ label = \"" ++ [c] ++ "\" ];"
              Nothing -> ";") $
       Set.toList sigma) ++
    "\n}"

newtype DFAState =
  DFAState Integer
  deriving (Eq, Ord)

instance Show DFAState where
  show (DFAState i) = "d" ++ show i

data DFA = DFA
    -- dfaStates       :: Set.Set DFAState -- wird berechnet
    -- dfaBigSigma     :: [Char]           -- wird berechnet
  { dfaSigma :: Set.Set ((DFAState, Char), DFAState)
  , dfaStart :: DFAState
  , dfaAcceptingStates :: Set.Set DFAState
  } deriving (Eq, Ord)

dfaStates :: DFA -> Set.Set DFAState
dfaStates dfa =
  Set.singleton (dfaStart dfa) `Set.union` dfaAcceptingStates dfa `Set.union`
  Set.unions
    (map (\((from, _), to) -> Set.fromList [from, to]) $
     Set.toList $ dfaSigma dfa)

dfaBigSigma :: DFA -> String
dfaBigSigma = Set.toList . Set.map (snd . fst) . dfaSigma

instance Show DFA
  -- Ausgabe in Datei speichern und mit [dot](http://www.graphviz.org/)
  -- umwandeln, z.B. `dot -Tpng -o dfa.png dfa.dot`
                                                    where
  show (DFA sigma start acceptingStates) =
    "digraph DFA {\n" ++
    "  rankdir=LR;\n" ++
    "  node [shape = point, color=white, fontcolor=white]; start;\n" ++
    "  node [shape = doublecircle, color=black, fontcolor=black];\n  " ++
    intercalate ";\n  " (map show $ Set.toList acceptingStates) ++
    ";\n" ++
    "  node [shape = circle];\n" ++
    "  start -> " ++
    show start ++
    ";\n  " ++
    intercalate
      "\n  "
      (map
         (\((from, c), to) ->
            show from ++ " -> " ++ show to ++ "[ label = \"" ++ [c] ++ "\" ];") $
       Set.toList sigma) ++
    "\n}"
