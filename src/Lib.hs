module Lib
    ( evalexpr
    ) where

import Control.Applicative
import Data.Char

data Expr = Add Expr Expr
            | Sub Expr Expr
            | Mul Expr Expr
            | Div Expr Expr
            | Pow Expr Expr
            | Lit Float

newtype Parser a  = Parser {
  parse :: String -> Either String (a, String)
}

--fmap :: (a -> b) -> Parser a -> Parser b
instance Functor Parser where
  fmap f pa = Parser p
      where
      p str = case (parse pa $ str) of
          Left err -> Left err
          Right (parsed, str') -> Right (f parsed, str')

-- liftA2 :: (a -> b -> c) -> f a -> f b -> f c
instance Applicative Parser where
  pure a = Parser $ \str -> Right (a, str)
  liftA2 fabc pa pb = Parser p
      where
          p str = case (parse pa $ str) of
              Left err -> Left err
              Right (parsed, str') -> case (parse pb $ str') of
                  Left err' -> Left err'
                  Right (parsed', str'') -> Right (fabc parsed parsed', str'')

-- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
-- return :: a -> Parser a
instance Monad Parser where
  return = pure
  pa >>= fapb = Parser b
      where
          b str = case (parse pa $ str) of
              Left e -> Left e
              Right (parsed, str') -> parse (fapb parsed) str'

              
-- (<|>) :: Parser a -> Parser a -> Parser a
-- empty :: Parser a
instance Alternative Parser where
  empty = Parser $ \_ -> Left "empty parser"
  pa <|> pa' = Parser p where
      p str = case (parse pa $ str) of
          Left _  -> (parse pa' $ str)
          Right x -> Right x

runParser :: Parser a -> String -> Either String a
runParser (Parser p) s = fst <$> p s

satisfy :: (Char -> Bool) -> Parser Char
satisfy pre = Parser p
  where
      p [] = Left "No char to parse"
      p (c:cs) = case (pre c) of
          True -> Right (c, cs)
          False -> Left "char is not satisfying predicate"

char :: Char -> Parser Char
char c = satisfy (c ==)

oneOf :: [Char] -> Parser Char
oneOf s = satisfy (`elem` s)

digit :: Parser Char
digit = oneOf ".0123456789"

integer :: Parser Float
integer = read <$> some digit

expr :: Parser Expr
expr = entry
    where
    entry = binOp Sub '-' entry1 <|> binOp Add '+' entry1 <|> entry1
    entry1 = binOp Sub '-' entry2 <|> binOp Add '+' entry2 <|> entry2
    entry2 = binOp Sub '-' term <|> binOp Add '+' term <|> term
    term = binOp Add '+' muldiv <|> binOp Sub '-' muldiv <|> muldiv
    muldiv = binOp Mul '*' muldiv1 <|> binOp Div '/' muldiv1 <|> muldiv1
    muldiv1 = binOp Mul '*' muldiv2 <|> binOp Div '/' muldiv2 <|> muldiv2
    muldiv2 = binOp Mul '*' power <|> binOp Div '/' power <|> power
    power = binOp Pow '^' factor <|> factor
    factor = parens <|> lit
    lit = Lit <$> integer
    parens = (char '(' *> expr <* char ')')
    --binop :: (a->a->b) -> Char -> Parser a -> Parser b
    binOp c o p = fmap c p <*> (char o *> p)

eval :: Expr -> Float
eval e = case e of
  Add a b -> eval a + eval b
  Sub a b -> eval a - eval b
  Mul a b -> eval a * eval b
  Pow a b -> eval a ** eval b
  Div a b -> eval a / eval b
  Lit n -> n

format :: String -> String
format str = foldl (\tab str' -> tab++str') [] (words str)

evalexpr :: String -> Either String Float
evalexpr s = (fmap eval) $ runParser expr (format s)