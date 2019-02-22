module Hash.Type where

data Expression =
    Block Expression Expression | -- expressions separated by ';'
    And Expression Expression | -- expr1 && expr2
    Or Expression Expression | -- expr1 || expr2
    Piped Expression Expression | -- expr1 | expr2
    Single String [String] -- cmd arg1 arg2
    deriving Show