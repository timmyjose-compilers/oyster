module Oyster.Core

import Data.Strings
import Oyster.Helper

%default total

-- the basic parser type

export
data Parser : Type -> Type where
  MkParser : (String -> List (a, String)) -> Parser a

export 
parse : Parser a -> String -> List (a, String)
parse (MkParser p) str = p str

-- the primitive parser

||| Parser that consumes a single character
export
item : Parser Char
item = MkParser (\str => case strM str of
                          StrNil => []
                          StrCons c cs => [(c, cs)])

-- make Parser a monad so that we can use the `do` 
-- notation (as well as making it fully generic)

export
Functor Parser where
  -- map : (a -> b) -> Parser a -> Parser b
  map f p = MkParser (\str => case parse p str of
                                [(x, rest)] => [(f x, rest)]
                                _ => [])

export
Applicative Parser where
  -- pure : a -> Parser a
  pure x = MkParser (\str => [(x, str)])

  -- (<*>) : Parser (a -> b) -> Parser a -> Parser b 
  pf <*> px = MkParser (\str => case parse pf str of
                                  [(f, rest)] => parse (map f px) rest
                                  _ => [])

export
Monad Parser where
  -- (>>=) : Parser a -> (a -> Parser b) -> Parser b
  p >>= f = MkParser (\str => case parse p str of
                                [(x, rest)] => parse (f x) rest
                                _ => [])

-- make the parser into an alternative for choice 
-- combination

partial
export
Alternative Parser where
  -- empty : Parser a
  empty = MkParser (\str => [])

  -- (<|>) : Parser a -> Parser a -> Parser a
  p <|> q = MkParser (\str => case parse p str of
                                [] => parse q str
                                [(x, rest)] => [(x, rest)])

mutual
  export partial
  some : Parser a -> Parser (List a)
  some x = pure (::) <*> x <*> many x

  export partial
  many : Parser a -> Parser (List a)
  many x = some x <|> pure []

-- derived primitives

sat : (Char -> Bool) -> Parser Char
sat p = do x <- item
           if p x then pure x else empty

digit : Parser Char
digit = sat isDigit

lower : Parser Char
lower = sat isLower

upper : Parser Char
upper = sat isUpper

letter : Parser Char
letter = sat isAlpha

alphanum : Parser Char
alphanum = sat isAlphaNum

char : Char -> Parser Char
char c = sat (== c)

partial
string : String -> Parser String
string str = stringHelper (strM str) where
  stringHelper : { s : String } -> StrM s -> Parser String
  stringHelper StrNil = pure ""
  stringHelper (StrCons c cs) = do char c 
                                   stringHelper (strM cs)
                                   pure (strCons c cs)

export partial
ident : Parser String
ident = do c <- lower
           cs <- many alphanum
           pure (listCharToString (c :: cs))

partial
nat : Parser Nat
nat = do ds <- some digit
         pure (stringToNatOrZ (listCharToString ds))

partial
space : Parser ()
space = do many (sat isSpace)
           pure ()

partial
int : Parser Int
int = do char '-'
         n <- nat
         pure (-(natToInt n))
      <|> do n <- nat
             pure (natToInt n)

export partial
token : Parser a -> Parser a
token p = do space
             v <- p
             space
             pure v

export partial
identifier : Parser String
identifier = token ident

export partial
natural : Parser Nat
natural = token nat

export partial
integer : Parser Int
integer = token int

export partial
symbol : String -> Parser String
symbol s = token (string s)

