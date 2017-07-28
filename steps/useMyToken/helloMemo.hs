import System.Environment

import Lexer

main :: IO ()
main = print . (fst <$>) . message . parse . concatMap lexer =<< getArgs

data Derivs = Derivs {
	message :: Maybe ((MyToken, MyToken), Derivs),
	greeting :: Maybe (MyToken, Derivs),
	name :: Maybe (MyToken, Derivs),
	token :: Maybe (MyToken, Derivs)
	}

parse :: [MyToken] -> Derivs
parse ts = d
	where
	d = Derivs m g n tkn
	m = pMessage d
	g = pGreeting d
	n = pName d
	tkn = case ts of
		(t : ts') -> Just (t, parse ts')
		_ -> Nothing

pMessage :: Derivs -> Maybe ((MyToken, MyToken), Derivs)
pMessage d = do
	(g, d') <- greeting d
	(n, d'') <- name d'
	return ((g, n), d'')

pGreeting :: Derivs -> Maybe (MyToken, Derivs)
pGreeting d = do
	(t, d') <- token d
	case t of
		Hello -> return (t, d')
		GoodBye -> return (t, d')
		_ -> fail "not parsed"

pName :: Derivs -> Maybe (MyToken, Derivs)
pName d = do
	(t, d') <- token d
	case t of
		World -> return (t, d')
		Yoshikuni -> return (t, d')
		_ -> fail "not parsed"
