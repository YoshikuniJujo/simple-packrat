{-# LANGUAGE QuasiQuotes #-}

import System.Environment

import MakeYjPap

main :: IO ()
main = do
	src : _ <- getArgs
	print . (fst <$>) . yj $ parse src

data Initial = Y | M | J deriving Show

[pap|

yj :: ([Initial], [Initial])
	= y:y j:j		{ (y, j) }
;
y :: [Initial]
	= m:('M':char	{ M })+ { m }
	/ y:('Y':char	{ Y })*	{ y }
;
j :: [Initial]
	= 'J':char js:j		{ J : js }
	/			{ [] }
;

|]
