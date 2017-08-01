{-# LANGUAGE TemplateHaskell, TupleSections #-}

module MakeYjPap (pap) where

import Control.Arrow
import Control.Monad
import qualified Data.List.NonEmpty as NE
import Data.Char
import Language.Haskell.TH
import Language.Haskell.TH.Quote

import ParseYjPap

pap :: QuasiQuoter
pap = QuasiQuoter {
	quoteExp = undefined,
	quotePat = undefined,
	quoteType = undefined,
	quoteDec = makePap
	}

makePap :: String -> Q [Dec]
makePap src = do
	ld <- listDec
	case parseYjPap src of
		Left e -> fail e
		Right r -> do
			p <- concat <$> mapM makeP r
			return $ makeDerivs r : makeParse r ++ p ++ ld

listDec :: Q [Dec]
listDec = [d|
	list, list1 :: MonadPlus m => m a -> m [a];
	list p = list1 p `mplus` return [];
	list1 p = (:) <$> p <*> list p
	|]

makeDerivs :: Rules -> Dec
makeDerivs rs = DataD [] (mkName "Derivs") [] Nothing [
	RecC (mkName "Derivs") $
		map makeDerivs1 rs ++ [(
			mkName "char",
			defBang,
			withDerivsType $ ConT ''Char )] ] []

makeDerivs1 :: Rule -> (Name, Bang, Type)
makeDerivs1 (Rule n t _) = (n, defBang, withDerivsType t)

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
makeP (Rule n t drs) = do
	does <- mapM (uncurry . makeDoE $ mkName "d") $ NE.toList drs
	return [
		mkTypeDec (putP n) t,
		FunD (putP n) [
			Clause	[VarP $ mkName "d"]
				(NormalB $ VarE 'msum `AppE` ListE does)
				[] ] ]

mkTypeDec :: Name -> Type -> Dec
mkTypeDec n t = SigD n $ ConT (mkName "Derivs") `arrT` withDerivsType t

makeDef :: Name -> Def -> Q ([Stmt], Name)
makeDef d ds = first concat <$> forStM makeDef1 d ds

makeDoE :: Name -> Def -> Result -> Q Exp
makeDoE dn d r = do
	(def, d') <- makeDef dn d
	return . DoE $ def ++ [
		NoBindS $ VarE 'return `AppE` TupE [r, VarE d'] ]

forStM :: Monad m => (s -> x -> m (y, s)) -> s -> [x] -> m ([y], s)
forStM f s0 (x : xs) = do
	(y, s1) <- f s0 x
	(ys, s') <- forStM f s1 xs
	return (y : ys, s')
forStM _ s0 [] = return ([], s0)

makeDef1 :: Name -> Def1 -> Q ([Stmt], Name)
makeDef1 d (p, Simple n) = do
	d' <- newName "d"
	return $ (, d') [
		BindS (TupP [p, VarP d']) $ VarE n `AppE` VarE d
		]
makeDef1 d (p, DefRslt lf dr) = do
	d' <- newName "d"
	doe <- uncurry (makeDoE d) dr
	let	l = case lf of
			Once -> VarE 'id
			List -> VarE $ mkName "list"
	return $ (, d') [
		BindS (TupP [p, VarP d']) $ l `AppE` doe
		]
