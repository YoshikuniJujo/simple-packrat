module Lexer (MyToken(..), lexer) where

import Data.List
import Data.Char

data MyToken = Hello | GoodBye | World | Yoshikuni deriving Show

lexer :: String -> [MyToken]
lexer = unfoldr lexer1

lexer1 :: String -> Maybe (MyToken, String)
lexer1 s = case span (not . isSpace) $ dropWhile isSpace s of
	("Hello", s') -> Just (Hello, s')
	("GoodBye", s') -> Just (GoodBye, s')
	("World", s') -> Just (World, s')
	("Yoshikuni", s') -> Just (Yoshikuni, s')
	_ -> Nothing
