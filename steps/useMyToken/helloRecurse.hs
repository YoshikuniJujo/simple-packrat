import System.Environment

import Lexer

main :: IO ()
main = print . pMessage . concatMap lexer =<< getArgs

pMessage :: [MyToken] -> Maybe ((MyToken, MyToken), [MyToken])
pMessage ts = case pGreeting ts of
	Just (g, ts') -> case pName ts' of
		Just (n, ts'') -> Just ((g, n), ts'')
		_ -> Nothing
	_ -> Nothing

pGreeting :: [MyToken] -> Maybe (MyToken, [MyToken])
pGreeting (Hello : ts) = Just (Hello, ts)
pGreeting (GoodBye : ts) = Just (GoodBye, ts)
pGreeting _ = Nothing

pName :: [MyToken] -> Maybe (MyToken, [MyToken])
pName (World : ts) = Just (World, ts)
pName (Yoshikuni : ts) = Just (Yoshikuni, ts)
pName _ = Nothing
