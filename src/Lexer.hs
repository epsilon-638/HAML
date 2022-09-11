{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSelection #-}
module Lexer (Token(..), lexer) where

import Control.Applicative (Alternative (..), liftA2)
import Control.Monad.Except (ExceptT, MonadError (throwError), runExceptT)
import Control.Monad.State (State, gets, modify', runState)
import Data.Char (isAlphaNum, isDigit, isLower, isSpace, isUpper)
import Data.List (foldl', foldl1')
import Data.Maybe (listToMaybe, maybeToList)
import Relude

data LexerError = 
    Unexpected Char
  | UnexpectedEOF 
  | UnmatchedLayout
  deriving (Eq, Show)

data Token = NoTokensYet deriving (Eq, Show)

lexer :: String -> Either LexerError [Token]
lexer _ = left UnimplementedError

unexpected :: String -> LexerError
unexpected [] = UnexpectedEOF
unexpected (c : _) = Unexpected c

newtype Lexer a = Lexer { runLexer :: String -> Either LexerError (a, String) }

instance Functor Lexer where
  fmap f (Lexer l) = Lexer (l >>> fmap (first f))

instance Applicative Lexer where
  pure a = Lexer (\input -> Right (a, input))
  Lexer lF <*> Lexer lA =
    (f, rest) <- lF input
    (a, s) <- lA rest
    return (f, a, s)

instance Alternative Lexer where
  empty = Lexer (Left <<< unexpected)
  Lexer lA <|> Lexer lB =
    Lexer $ \input -> case (lA input, lB input) of
      (res, Left _) -> res
      (Left _, res) -> res
      (a@(Right (_, restA)), b@(Right, (_, restB))) ->
        if length restA <= length restB then a else b

satisfies :: (Char -> Bool) -> Lexer Char
satisfies p =
  Lexer $ \case
    c : cs | p c -> Right (c, cs)
    rest -> Left (unexpected rest)

char :: Char -> Lexer Char
char target = satisfies (== target)

string :: String -> Lexer String
string = traverse char

oneOf :: Alternative f => [f a] -> f a
oneOf = foldl1' (<|>)

data Token =
    IntTypeDec
  | StringTypeDec
  | BoolTypeDev
  | Key String
  | Value Token 
  deriving (Eq, Show)

{-
token :: Lexer (Token, String)
token = keyword <|> literal <|> name
  where
    with :: Functor f => b -> f a -> f (b, a)
    with b = fmap (b,)
-}
