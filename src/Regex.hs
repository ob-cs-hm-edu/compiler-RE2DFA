module Regex
  ( str2re
  ) where

import           Control.Applicative ((<$>))
import           Data.Char           (isLetter)
import           Types

data REToken = Character Char
             | OpeningParen
             | ClosingParen
             | Star
             | Alternative
  deriving (Eq, Show)

str2re :: String -> Maybe RE
str2re str = scan str >>= parse

scan :: String -> Maybe [REToken]
scan [] = Just []
scan (x:xs)
  | x == '('   = (OpeningParen:) <$> scan xs
  | x == ')'   = (ClosingParen:) <$> scan xs
  | x == '*'   = (Star:)         <$> scan xs
  | x == '|'   = (Alternative:)  <$> scan xs
  | isLetter x = (Character x:)  <$> scan xs
  | otherwise  = Nothing

parse :: [REToken] -> Maybe RE
parse []              = Nothing
parse (Star:_)        = Nothing
parse (Alternative:_) = Nothing
parse token           = case parse' [] 0 token of
  -- parsen ist nur dann erfolgreich wenn der gesamte Tokenstrom
  -- verbraucht ist und keine Klammer mehr offen ist
  (re, [], 0) -> Just re
  -- sonst ist das Parsen nicht erfolgreich gewesen
  _           -> Nothing

parse' :: [RE]      -- bisher hintereinander gefundene (konkatenierte)
                    -- REs in umgekehrter Reihenfolge (Zugriff vorne is
                    -- schnell)
       -> Integer   -- Anzahl von momentan gesuchten schließenden Klammern
       -> [REToken] -- Tokenliste
       -> (RE, [REToken], Integer)

-- wenn die Liste der Tokens leer ist, werden die gefundenen REs zusammengesetzt
parse' res n [] = (concatenateREs res, [], n)

-- Ein Character wird zu einem primitiven RE, er wird vor die
-- bereits davor gefundenen REs gestellt
parse' res n (Character c : rest) = parse' (PrimitiveRE c : res) n rest

-- Öffnende Klammer gefunden, wobei schon n schließende gesucht werden
parse' res n tokens@(OpeningParen:rest) =
  -- Wir parsen jetzt innerhalb der Klammer ohne die zuvor gefundenen REs,
  -- und suchen nach einer schließenden Klammer mehr
  let (re, rest', n') = parse' [] (n+1) rest
     -- wurde genau eine Klammer gefunden?
  in if n == n'
     -- dann füge den im Klammernpaar gefundenen RE an die vorher gefunden
     -- REs vorne an und parse den Rest, hinter der schließenden Klammer.
     then parse' (re:res) n' rest'
     -- wenn keine Klammer gefunden wurde, gebe den vor der öffnenden Klammer
     -- berechneten Stand zurück
     else (concatenateREs res, tokens, n)

-- Wenn wir eine schließende Klammer finden, aber gar keine suchen, geben
-- wir die konkatenierten REs mit den unverbrauchten Tokens, inkl. der
-- gerade gefundenen Klammer zurück. Dies markiert das Ende eines geklammerten
-- Ausdrucks, den wir suchen, weil wir zuvor eine Alternative gefunden haben.
parse' res 0 tokens@(ClosingParen:_) = (concatenateREs res, tokens, 0)

-- Wenn wir eine schließende Klammer gefunden haben und gerade n suchen,
-- suchen wir jetzt nur noch n-1 und geben das Ergebnis bis dahin zurück
parse' res n (ClosingParen:rest) = (concatenateREs res, rest, n-1)

-- Wenn wir einen Stern finden, bezieht er sich nur auf den zuvor gefundenen
-- RE.
parse' (re:res) n (Star:rest) = parse' (ClosureRE re:res) n rest
parse' [] _ (Star:_) = error "this should not happen"

parse' res n (Alternative:rest) =
  -- Wenn wir eine Alternative  gefunden haben, machen wir uns auf die Suche nach
  -- dem Ende des Teilausdrucks nach der Alternative. Das Ende entspricht entweder
  -- dem Ende der gesamten Tokenliste oder liegt unittelbar vor der schließenden
  -- Klammer, die zur letzten noch nicht geschlossenen öffnenden Klammer gehört.
  -- Wir setzen daher zunächst die Anzahl der gesuchten Klammern auf 0. Die
  -- schließende Klammer bleibt dann als Kopf der noch nicht verbrauchten
  -- Tokenliste.
  let (re, rest', _) = parse' [] 0 rest
  -- Anschließend machen wir uns wieder auf die Suche nach Klammern.
  in parse' [AlternativeRE (concatenateREs res) re] n rest'

-- eine Liste von REs wird zu einer Kaskade von ConcatenatedREs.
-- die Liste die die REs in umgekehrter Reihenfolge speichert, muss zuerst
-- umgedreht werde
-- foldr1 verbindet die REs mit ConcatenatedRE
-- Achtung: Wird eine leere Liste übergeben, führt das zum Absturz mit
-- folgender Fehlermeldung:
-- *** Exception: Prelude.foldr1: empty list
-- Das passiert wenn im eingegebenen RE ein leeres Klammerpaar vorkommt.
concatenateREs :: [RE] -> RE
concatenateREs = foldr1 ConcatenatedRE . reverse
