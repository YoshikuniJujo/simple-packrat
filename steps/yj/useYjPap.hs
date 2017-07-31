{-# LANGUAGE QuasiQuotes #-}

import System.Environment

import MakeYjPap

main :: IO ()
main = do
	src : _ <- getArgs
	print . (fst <$>) . yj $ parse src

data Initial = Y | M | J deriving Show

[pap|

yj :: (Initial, Initial)
	= y:y j:j		{ (y, j) }
;
y :: Initial
	= 'Y':char		{ Y }
	/ 'M':char		{ M }
;
j :: Initial
	= 'J':char		{ J }
;

|]
