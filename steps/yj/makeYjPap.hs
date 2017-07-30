{-# LANGUAGE TemplateHaskell #-}

import Language.Haskell.TH

import ParseYjPap

main :: IO ()
main = do
	str <- getContents
	case parseYjPap str of
		Left e -> putStrLn e
		Right r -> do
			putStrLn "RULES"
			print r
			putStrLn "\nDerivs"
			print . ppr $ makeDerivs r

makeDerivs :: Rules -> Dec
makeDerivs rs = DataD [] (mkName "Derivs") [] Nothing [
	RecC (mkName "Derivs") $
		map makeDerivs1 rs ++ [(
			mkName "char",
			defBang,
			withDerivsType $ ConT ''Char )] ] []

makeDerivs1 :: Rule -> (Name, Bang, Type)
makeDerivs1 (Rule n t _ _) = (n, defBang, withDerivsType t)

withDerivsType :: Type -> Type
withDerivsType t = ConT ''Maybe `AppT` (t `tupleType` ConT (mkName "Derivs"))

tupleType :: Type -> Type -> Type
tupleType t1 t2 = TupleT 2 `AppT` t1 `AppT` t2

defBang :: Bang
defBang = Bang NoSourceUnpackedness NoSourceStrictness
