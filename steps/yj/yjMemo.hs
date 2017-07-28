import System.Environment

main :: IO ()
main = print . (fst <$>) . yj . parse . head =<< getArgs

data Initial = Y | J deriving Show

data Derivs = Derivs {
	yj :: Maybe ((Initial, Initial), Derivs),
	y :: Maybe (Initial, Derivs),
	j :: Maybe (Initial, Derivs),
	char :: Maybe (Char, Derivs) }

parse :: String -> Derivs
parse s = d
	where
	d = Derivs yj' y' j' c'
	yj' = pYj d
	y' = pY d
	j' = pJ d
	c' = case s of
		(c : cs) -> Just (c, parse cs)
		_ -> Nothing

pYj :: Derivs -> Maybe ((Initial, Initial), Derivs)
pYj d = do
	(y', d') <- y d
	(j', d'') <- j d'
	return ((y', j'), d'')

pY :: Derivs -> Maybe (Initial, Derivs)
pY d = do
	(y', d') <- char d
	case y' of
		'Y' -> return (Y, d')
		_ -> fail "not parsed"

pJ :: Derivs -> Maybe (Initial, Derivs)
pJ d = do
	(j', d') <- char d
	case j' of
		'J' -> return (J, d')
		_ -> fail "not parsed"
