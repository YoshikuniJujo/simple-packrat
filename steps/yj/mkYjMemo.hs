{-# LANGUAGE TemplateHaskell #-}

import Data.Char
import Language.Haskell.TH

main :: IO ()
main = do
	runQ decs >>= print . ppr

decs :: Q [Dec]
decs = return $ [ derivDec ] ++ parseDec

defBang :: Bang
defBang = Bang NoSourceUnpackedness NoSourceStrictness

derivDec :: Dec
derivDec = DataD [] (mkName "Derivs") [] Nothing
			[RecC (mkName "Derivs") [
				(	mkName "yj", defBang,
					maybeType $ withDerivsType iniIniType ),
				(	mkName "y", defBang,
					maybeType $ withDerivsType iniType ),
				(	mkName "j", defBang,
					maybeType $ withDerivsType iniType ),
				(	mkName "char", defBang,
					maybeType $ withDerivsType charType )
				]] []

iniType :: Type
iniType = ConT $ mkName "Initial"

iniIniType :: Type
iniIniType = iniType `tupleType` iniType

charType :: Type
charType = ConT ''Char

maybeType :: Type -> Type
maybeType t = ConT ''Maybe `AppT` t

tupleType :: Type -> Type -> Type
tupleType t1 t2 = TupleT 2 `AppT` t1 `AppT` t2

withDerivsType :: Type -> Type
withDerivsType t = t `tupleType` ConT (mkName "Derivs")

parseDec :: [Dec]
parseDec = [
	SigD (mkName "parse") $ ConT ''String `arrT` ConT (mkName "Derivs"),
	FunD (mkName "parse") [
		Clause	[VarP $ mkName "s"]
			(NormalB . VarE $ mkName "d") [
				ValD (VarP $ mkName "d") (NormalB derivsE) [],
				mkValD "yj", mkValD "y", mkValD "j",
				ValD (VarP $ mkName "c'") (NormalB $ caseS) []
				]
		]
	]
	where
	derivsE = ConE (mkName "Derivs") `AppE`
		VarE (mkName "yj'") `AppE`
		VarE (mkName "y'") `AppE`
		VarE (mkName "j'") `AppE`
		VarE (mkName "c'")
	mkValD n = ValD
		(VarP . mkName $ n ++ "'")
		(NormalB $ VarE (mkName $ 'p' : capitalize n) `AppE`
			VarE (mkName "d"))
		[]
	caseS = CaseE (VarE $ mkName "s") [
		Match (InfixP (VarP $ mkName "c") '(:) (VarP $ mkName "cs"))
			(NormalB $ VarE 'Just `AppE`
				TupE [	VarE $ mkName "c",
					VarE (mkName "parse") `AppE`
						VarE (mkName "cs")
					]) [],
		Match WildP (NormalB $ VarE 'Nothing) []
		]

capitalize :: String -> String
capitalize (h : t) = toUpper h : t
capitalize "" = ""

arrT :: Type -> Type -> Type
t1 `arrT` t2 = ArrowT `AppT` t1 `AppT` t2
