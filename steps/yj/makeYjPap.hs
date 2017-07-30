{-# LANGUAGE TemplateHaskell #-}

import Data.Char
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
			putStrLn "\nparse"
			print . ppr $ makeParse r

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

makeParse :: Rules -> [Dec]
makeParse rs = [
	SigD (mkName "parse") $ ConT ''String `arrT` ConT (mkName "Derivs"),
	FunD (mkName "parse") [
		Clause	[VarP $ mkName "s"]
			(NormalB . VarE $ mkName "d") (
				ValD (VarP $ mkName "d") (NormalB derivsE) [] :
					map (mkValD . ruleName) rs ++
					[caseValD]
				)
		]
	]
	where
	derivsE = ConE (mkName "Derivs") `takeArgs`
		(map (VarE . primed . ruleName) rs ++ [VarE $ mkName "c'"])
	mkValD n = ValD
		(VarP $ primed n)
		(NormalB $ VarE (putP n) `AppE` VarE (mkName "d"))
		[]
	caseValD = ValD (VarP $ mkName "c'") (NormalB caseS) []
	caseS = CaseE (VarE $ mkName "s") [
		Match (InfixP (VarP $ mkName "c") '(:) (VarP $ mkName "cs"))
			(NormalB $ ConE 'Just `AppE`
				TupE [	VarE $ mkName "c",
					VarE (mkName "parse") `AppE`
						VarE (mkName "cs")
					]) [],
		Match WildP (NormalB $ ConE 'Nothing) []
		]

primed :: Name -> Name
primed n = mkName $ nameBase n ++ "'"

putP :: Name -> Name
putP n = case nameBase n of
	c : cs -> mkName $ 'p' : toUpper c : cs
	"" -> mkName "p"

arrT :: Type -> Type -> Type
t1 `arrT` t2 = ArrowT `AppT` t1 `AppT` t2

takeArgs :: Exp -> [Exp] -> Exp
takeArgs = foldl AppE
