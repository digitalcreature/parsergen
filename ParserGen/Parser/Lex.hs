module ParserGen.Parser.Lex where

import ParserGen.Parser.Types

import Data.Functor
import Control.Applicative
import Control.Monad

import Text.Parsec


data Lex a = Lex (Maybe SourcePos) a
type LexParse a = Parse (Lex a)

instance Functor Lex where
  fmap f (Lex p a) = Lex p (f a)

instance Applicative Lex where
  pure a = Lex Nothing a
  (Lex p f) <*> (Lex _ a) = Lex p (f a)

pLex :: Parse a -> LexParse a
pLex pa = do
  s <- getPosition
  a <- pa
  return $ Lex (Just s) a

val :: Lex a -> a
val (Lex _ a) = a

pos :: Lex a -> Maybe SourcePos
pos (Lex p _) = p
