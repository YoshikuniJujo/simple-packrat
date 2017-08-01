import System.Environment
import Control.Monad.State

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
pYj = runStateT $ do
	y' <- StateT y
	j' <- StateT j
	return (y', j')

pY :: Derivs -> Maybe (Initial, Derivs)
pY = runStateT $ do
	y' <- StateT char
	case y' of
		'Y' -> return Y
		_ -> fail "not parsed"

pJ :: Derivs -> Maybe (Initial, Derivs)
pJ = runStateT $ do
	j' <- StateT char
	case j' of
		'J' -> return J
		_ -> fail "not parsed"
