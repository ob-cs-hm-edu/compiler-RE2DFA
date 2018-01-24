module Main where

import Data.Semigroup ((<>))
import Hopcroft (dfa2min)
import Options.Applicative
import Regex (str2re)
import SubsetConstruction (nfa2dfa)
import ThompsonsConstruction (re2nfa)

data Command
  = ShowRegex
  | ShowNFA
  | ShowDFA
  | ShowMinimalDFA
  deriving (Eq)

data Config = Config
  { optCommand :: Command
  , regex :: String
  }

config =
  Config <$>
  hsubparser
    (command "regex" (info (pure ShowRegex) (progDesc "regex ausgeben")) <>
     command "nfa" (info (pure ShowNFA) (progDesc "NFA ausgeben")) <>
     command "dfa" (info (pure ShowDFA) (progDesc "DFA ausgeben")) <>
     command
       "mindfa"
       (info (pure ShowMinimalDFA) (progDesc "Minimalen DFA ausgeben"))) <*>
  argument str (metavar "REGEX" <> help "regulärer Ausdruck")

main :: IO ()
main = main' =<< execParser opts
  where
    opts =
      info
        (config <**> helper)
        (fullDesc <> progDesc "Umwandlung eines Regex in einen minimalen DFA" <>
         header "re2dfa")

main' config = do
  let maybeRegex = str2re $ regex config
  case maybeRegex of
    Nothing -> error "Regex lässt sich nicht parsen"
    Just re ->
      case optCommand config of
        ShowRegex -> print re
        ShowNFA -> print $ re2nfa re
        ShowDFA -> print $ nfa2dfa $ re2nfa re
        ShowMinimalDFA -> print $ dfa2min $ nfa2dfa $ re2nfa re
