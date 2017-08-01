{-# LANGUAGE TemplateHaskell, TupleSections, LambdaCase #-}

module MakeYjPap (pap) where

import Control.Monad
import Control.Monad.State
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
	list1 p = (:) <$> p <*> list p;

	optional :: MonadPlus m => m a -> m (Maybe a);
	optional p = (Just <$> p) `mplus` return Nothing
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
	does <- mapM (uncurry makeDoE) $ NE.toList drs
	return [
		mkTypeDec (putP n) t,
		ValD (VarP $ putP n) (
			NormalB $ VarE 'runStateT `AppE`
				(VarE 'msum `AppE` ListE does) )
				[] ]

mkTypeDec :: Name -> Type -> Dec
mkTypeDec n t = SigD n $ ConT (mkName "Derivs") `arrT` withDerivsType t

makeDoE :: Def -> Result -> Q Exp
makeDoE d r = do
	def <- makeDef d
	return . DoE $ def ++ [NoBindS $ VarE 'return `AppE` r]

makeDef :: Def -> Q [Stmt]
makeDef ds = concat <$> mapM makeDef1 ds

makeDef1 :: Def1 -> Q [Stmt]
makeDef1 (p, Simple n, mg, lf) = return $
	[BindS p $ makeListify lf `AppE` (ConE 'StateT `AppE` VarE n)] ++
	makeGuard mg
makeDef1 (p, DefRslt dr, mg, lf) = do
	doe <- uncurry makeDoE dr
	return $ [BindS p $ makeListify lf `AppE` doe] ++ makeGuard mg

makeListify :: Listify -> Exp
makeListify = \case
	Once -> VarE 'id
	List -> VarE $ mkName "list"
	List1 -> VarE $ mkName "list1"
	Option -> VarE $ mkName "optional"

makeGuard :: Maybe Grd -> [Stmt]
makeGuard (Just g) = [NoBindS $ VarE 'guard `AppE` g]
makeGuard Nothing = []
