{-# LANGUAGE QuasiQuotes #-}

import Language.Haskell.TH
import Language.Haskell.TH.Quote
import System.Environment

import MakeYjPap

main :: IO ()
main = do
	src : _ <- getArgs
	print . (fst <$>) . yj $ parse src

data Initial = Y | J deriving Show

[pap|

yj :: (Initial, Initial)
	= y:y j:j		{ (y, j) }
;
y :: Initial
	= 'Y':char		{ Y }
;
j :: Initial
	= 'J':char		{ J }
;

|]
