module ParserGen.Parser.Token where

import ParserGen.Parser.Types
import ParserGen.Parser.Lex

import Text.Parsec

symbol :: String -> LexParse String
symbol s = pLex $ (string s) <* spaces
