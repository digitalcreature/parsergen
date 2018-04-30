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

instance Show RuleName where
  show (RuleName name) = name

instance Show Literal where
  show (Literal value) = show value

instance Show Grammar where
  show (Grammar rules) = "[" ++ (showr rules) ++ "]"
    where
      showr [r] = show (val r)
      showr (r:rs) = (show (val r))  ++ "\n" ++ (showr rs)

instance Show Rule where
  show (Rule (Lex _ name) (Lex _ expr)) = (show name) ++ " -> \n\t" ++ (show expr)

instance Show Espression where
  show (EOption es) = showe
    where
      showe [e] = show (val e)
      showe (e:es) = (show (val e)) ++ " | " ++ (show rs)
  show 
