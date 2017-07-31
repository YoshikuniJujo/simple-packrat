{-# LANGUAGE TemplateHaskell, TupleSections #-}

import Control.Arrow
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
			putStrLn "\np functions"
			print . ppr . concat =<< mapM (runQ . makeP) r

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

makeP :: Rule -> Q [Dec]
makeP (Rule n t d r) = do
	(def, d') <- makeDef d
	return [
		mkTypeDec n t,
		FunD n [Clause [VarP $ mkName "d"] (NormalB . DoE $ def ++
			[
				NoBindS $ VarE 'return `AppE` TupE [
					r, VarE $ mkName "d" ] ]) [] ]
		]

mkTypeDec :: Name -> Type -> Dec
mkTypeDec n t = SigD n $ ConT (mkName "Derivs") `arrT` withDerivsType t

makeDef :: Def -> Q ([Stmt], Name)
makeDef ds = first concat <$> forStM makeDef1 (mkName "d") ds

forStM :: Monad m => (s -> x -> m (y, s)) -> s -> [x] -> m ([y], s)
forStM f s0 (x : xs) = do
	(y, s1) <- f s0 x
	(ys, s') <- forStM f s1 xs
	return (y : ys, s')
forStM _ s0 [] = return ([], s0)

makeDef1 :: Name -> Def1 -> Q ([Stmt], Name)
makeDef1 d (p, n) = do
		d' <- newName "d"
		t <- newName "t"
		return $ (, d') [
			BindS (TupP [VarP t, VarP d']) $ VarE n `AppE` VarE d
			]

nameDs :: [Name]
nameDs = iterate primed $ mkName "d"
