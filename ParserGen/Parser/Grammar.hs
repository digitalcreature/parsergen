module ParserGen.Parser.Grammar where

import ParserGen.Parser.Token
import ParserGen.Parser.AST
import ParserGen.Parser.Lex

import Control.Monad

import Text.Parsec
import Text.Parsec.Char

-- data RuleName = RuleName String
pRuleName :: LexParse RuleName
pRuleName = pLex $ liftM (RuleName) (many1 lower)

-- data Literal = Literal String
pLiteral :: LexParse Literal
pLiteral = pLex $ liftM (Literal) ((char '\'') *> (many $ noneOf $ "'") <* symbol "'")

-- data Grammar = Grammar [Lex Rule]
pGrammar :: LexParse Grammar
pGrammar = pLex $ liftM (Grammar) (many pRule)

-- data Rule = Rule (Lex RuleName) (Lex Expression)
pRule :: LexParse Rule
pRule = pLex $ do
  name <- pRuleName
  symbol "->"
  expr <- pExpression
  return $ Rule name expr

pExpression :: LexParse Expression
pExpression = undefined

-- data Expression =
--   EOption [Lex Expression]
--   EOptional Lex Expression
--   EMany Lex Expression
--   EMany1 Lex Expression
--   ERule Lex RuleName
--   ELiteral Lex Literal
