module ParserGen.Parser.AST where

import ParserGen.Parser.Lex

data RuleName = RuleName String
data Literal = Literal String

data Grammar = Grammar [Lex Rule]

data Rule = Rule (Lex RuleName) (Lex Expression)

data Expression
  = EOption [Lex Expression]
  | EOptional (Lex Expression)
  | EMany (Lex Expression)
  | EMany1 (Lex Expression)
  | ERule (Lex RuleName)
  | ELiteral (Lex Literal)
