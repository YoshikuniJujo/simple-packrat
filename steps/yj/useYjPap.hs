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
	= 'I':char		{ I }
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
