import System.Environment

import Lexer

main :: IO ()
main = print . pMessage . concatMap lexer =<< getArgs

pMessage :: [MyToken] -> Maybe ((MyToken, MyToken), [MyToken])
pMessage ts = do
	(g, ts') <- pGreeting ts
	(n, ts'') <- pName ts'
	return ((g, n), ts'')

pGreeting :: [MyToken] -> Maybe (MyToken, [MyToken])
pGreeting (Hello : ts) = Just (Hello, ts)
pGreeting (GoodBye : ts) = Just (GoodBye, ts)
pGreeting _ = Nothing

pName :: [MyToken] -> Maybe (MyToken, [MyToken])
pName (World : ts) = Just (World, ts)
pName (Yoshikuni : ts) = Just (Yoshikuni, ts)
pName _ = Nothing
