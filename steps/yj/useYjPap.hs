{-# LANGUAGE QuasiQuotes #-}

import System.Environment

import MakeYjPap

main :: IO ()
main = do
	src : _ <- getArgs
	print . (fst <$>) . yj $ parse src

data Initial = Y | M | I | J deriving Show

[pap|

yj :: (Maybe Initial, [Initial], [Initial])
	= i:i? y:y j:j		{ (i, y, j) }
;
i :: Initial
	= 'I'			{ I }
;
y :: [Initial]
	= m:('M' { M })+	{ m }
	/ y:('Y' { Y })*	{ y }
;
j :: [Initial]
	= 'J' js:j		{ J : js }
	/			{ [] }
;

|]
