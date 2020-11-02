module Main

import Oyster.Core

%default total

{-
  EBNF grammar:

  expr ::= term ( + expr | - expr | epsilon )
  term ::= factor ( * expr | / expr | epsilon )
  factor ::= ( expr ) | integer
  integer ::= ... -2 | -1 | 0 | 1 | 2 | ...
-}

mutual
  partial
  expr : Parser Int
  expr = do t <- term
            do symbol "+"
               e <- expr
               pure (t + e)
             <|> do symbol "-"
                    e <- expr
                    pure (t - e)
             <|> pure t


  partial
  term : Parser Int
  term = do f <- factor
            do symbol "*"
               t <- term
               pure (f * t)
             <|> do symbol "/"
                    t <- term
                    pure (f `div` t)
             <|> pure f

  partial
  factor : Parser Int
  factor = do symbol "("
              e <- expr
              symbol ")"
              pure e
            <|> integer

eval : String -> Maybe Int
eval inp = case parse expr inp of
                [(n, "")] => Just n
                _ => Nothing

main : IO ()
main = do expr <- getLine
          pure ()
