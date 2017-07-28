{-# LANGUAGE QuasiQuotes, TypeFamilies #-}

import Text.Papillon

data Initial = Y | J deriving Show

[papillon|

yj :: (Initial, Initial)
	= y:y j:j		{ (y, j) }

y :: Initial
	= 'Y':char		{ Y }

j :: Initial
	= 'J':char		{ J }

|]
