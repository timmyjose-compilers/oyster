module Oyster.Helper

import Data.Strings

%default total

export
listCharToString : List Char -> String
listCharToString [] = ""
listCharToString (c :: cs) = strCons c (listCharToString cs)

export
natToInt : Nat -> Int
natToInt Z = 0
natToInt (S k) = 1 + natToInt k