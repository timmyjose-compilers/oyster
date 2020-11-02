module ParseInteger

import Oyster.Core

nats : Parser (List Int)
nats = do symbol "["
          d <- integer
          ds <- many (do symbol "," ; integer)
          symbol "]"
          pure (d :: ds)

partial
main : IO ()
main =  do input <- getLine
           printLn (parse nats input)