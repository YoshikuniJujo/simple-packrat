{-# LANGUAGE QuasiQuotes, TypeFamilies #-}

import Text.Papillon
import System.Environment

import Lexer

main :: IO ()
main = do
	as <- getArgs
	case runError . hello . parse $ concatMap lexer as of
		Left e -> error $ peMessage e
		Right (h, _) -> print h

instance SourceList MyToken where
	data ListPos MyToken = MyPos Int
	listToken [] = Nothing
	listToken (c : s) = Just (c, s)
	listInitialPos = MyPos 0
	listUpdatePos _ (MyPos n) = MyPos $ n + 1

[papillon|

source: [MyToken]

hello :: (MyToken, MyToken)
	= g:greeting n:name	{ (g, n) }

greeting :: MyToken
	= Hello			{ Hello }
	/ GoodBye		{ GoodBye }

name :: MyToken
	= World			{ World }
	/ Yoshikuni		{ Yoshikuni }

|]
