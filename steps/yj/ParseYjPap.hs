{-# LANGUAGE QuasiQuotes, TypeFamilies #-}

module ParseYjPap (
	Rules, Rule(..), Def, Def1, Result, Nest(..), Listify(..),
	parseYjPap, ruleName) where

import Data.Char
import Data.List.NonEmpty
import Text.Papillon
import Language.Haskell.TH

parseYjPap :: String -> Either String Rules
parseYjPap src = case runError . rules $ parse src of
	Left e -> Left $ peMessage e
	Right (r, _) -> Right r

type Rules = [Rule]

data Rule = Rule Name Type (NonEmpty (Def, Result)) deriving Show

ruleName :: Rule -> Name
ruleName (Rule n _ _) = n

type Def = [Def1]
type Def1 = (Pat, Nest, Listify)
type Result = Exp

data Nest
	= Simple Name
	| DefRslt (Def, Result)
	deriving Show

data Listify = Once | List | List1 | Option deriving Show

[papillon|

rules :: Rules
	= rs:( _:spaces r:rule _:spaces ';' { r })*
					{ rs }

rule :: Rule
	= n:<isLower>+ _:spaces ':' ':' _:spaces
		t:typ _:spaces '=' _:spaces drs:defRslts
					{ Rule (mkName n) t drs }

typ :: Type
	= t1:typ1 _:spaces t2:typ1	{ t1 `AppT` t2 }
	/ t:typ1			{ t }

typ1 :: Type
	= c:<isUpper> cs:<isLower>*	{ ConT . mkName $ c : cs }
	/ '(' _:spaces t1:typ _:spaces ',' _:spaces t2:typ _:spaces ')'
					{ TupleT 2 `AppT` t1 `AppT` t2 }
	/ '(' _:spaces t1:typ _:spaces ',' _:spaces
		t2:typ _:spaces ',' _:spaces
		t3:typ _:spaces ')'
					{ TupleT 3 `AppT` t1 `AppT`
						t2 `AppT` t3 }
	/ '[' t:typ ']'			{ ListT `AppT` t }

defRslts :: NonEmpty (Def, Result)
	= dr:defRslt drs:(_:spaces '/' _:spaces drs:defRslt { drs })*
					{ dr :| drs }

defRslt :: (Def, Result)
	= d:def _:spaces '{' _:spaces r:expr _:spaces '}'
					{ (d, r) }

def :: Def
	= (p, n):def1 l:listify _:spaces ds:def
					{ (p, n, l) : ds }
	/				{ [] }

def1 :: (Pat, Nest)
	= p:pat ':' d:<isLower>+	{ (p, Simple $ mkName d) }
	/ p:pat ':' '(' _:spaces dr:defRslt _:spaces ')'
					{ (p, DefRslt dr) }

listify :: Listify
	= '*'				{ List }
	/ '+'				{ List1 }
	/ '?'				{ Option }
	/				{ Once }

pat :: Pat
	= p:<isLower>+			{ VarP $ mkName p }
	/ '\'' c:<(/= '\'')> '\''	{ LitP $ CharL c }

spaces :: ()
	= _:<isSpace>*

expr :: Exp
	= e:expr1 _:spaces ':' _:spaces es:expr1
					{ ConE (mkName ":") `AppE` e `AppE` es }
	/ e:expr1			{ e }

expr1 :: Exp
	= c:<isUpper> cs:<isAlpha>*	{ ConE . mkName $ c : cs }
	/ c:<isLower> cs:<isAlpha>*	{ VarE . mkName $ c : cs }
	/ '\'' c:<(/= '\'')> '\''	{ LitE $ CharL c }
	/ '(' _:spaces e1:expr _:spaces ',' _:spaces e2:expr _:spaces ')'
					{ TupE [e1, e2] }
	/ '(' _:spaces e1:expr _:spaces ',' _:spaces
		e2:expr _:spaces ',' _:spaces
		e3:expr _:spaces ')'
					{ TupE [e1, e2, e3] }
	/ '[' ']'			{ ConE $ mkName "[]" }

|]
