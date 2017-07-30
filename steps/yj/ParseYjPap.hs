{-# LANGUAGE QuasiQuotes, TypeFamilies #-}

module ParseYjPap (
	parseYjPap,
	Rules, Rule(..), Def, Def1, Result) where

import Data.Char
import Text.Papillon
import Language.Haskell.TH

parseYjPap :: String -> Either String Rules
parseYjPap src = case runError . rules $ parse src of
	Left e -> Left $ peMessage e
	Right (r, _) -> Right r

type Rules = [Rule]

data Rule = Rule Name Type Def Result deriving Show

type Def = [Def1]
type Def1 = (Pat, Name)
type Result = Exp

[papillon|

rules :: Rules
	= r:rule _:spaces ';' _:spaces rs:rules		
					{ r : rs }
	/				{ [] }

rule :: Rule
	= n:<isLower>+ _:spaces ':' ':' _:spaces
		t:typ _:spaces '=' _:spaces d:def _:spaces
		'{' _:spaces r:expr _:spaces '}'
					{ Rule (mkName n) t d r }

typ :: Type
	= c:<isUpper> cs:<isLower>*	{ ConT . mkName $ c : cs }
	/ '(' _:spaces t1:typ _:spaces ',' _:spaces t2:typ _:spaces ')'
					{ TupleT 2 `AppT` t1 `AppT` t2 }

def :: Def
	= d:def1 _:spaces ds:def	{ d : ds }
	/				{ [] }

def1 :: Def1
	= p:pat ':' d:<isLower>+	{ (p, mkName d) }

pat :: Pat
	= p:<isLower>+			{ VarP $ mkName p }
	/ '\'' c:<(/= '\'')> '\''	{ LitP $ CharL c }

spaces :: ()
	= _:<isSpace>*

expr :: Exp
	= c:<isUpper> cs:<isAlpha>*	{ ConE . mkName $ c : cs }
	/ c:<isLower> cs:<isAlpha>*	{ VarE . mkName $ c : cs }
	/ '\'' c:<(/= '\'')> '\''	{ LitE $ CharL c }
	/ '(' _:spaces e1:expr _:spaces ',' _:spaces e2:expr _:spaces ')'
					{ TupE [e1, e2] }

|]
