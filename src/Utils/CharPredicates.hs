module Utils.CharPredicates where

isNewline :: Char -> Bool
isNewline x = x `elem` "\r\n"