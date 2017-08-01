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
type Def1 = (Pat, Nest)
type Result = Exp

data Nest
	= Simple Name
	| DefRslt Listify (Def, Result)
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
	= c:<isUpper> cs:<isLower>*	{ ConT . mkName $ c : cs }
	/ '(' _:spaces t1:typ _:spaces ',' _:spaces t2:typ _:spaces ')'
					{ TupleT 2 `AppT` t1 `AppT` t2 }
	/ '[' t:typ ']'			{ ListT `AppT` t }

defRslts :: NonEmpty (Def, Result)
	= dr:defRslt drs:(_:spaces '/' _:spaces drs:defRslt { drs })*
					{ dr :| drs }

defRslt :: (Def, Result)
	= d:def _:spaces '{' _:spaces r:expr _:spaces '}'
					{ (d, r) }

def :: Def
	= d:def1 _:spaces ds:def	{ d : ds }
	/				{ [] }

def1 :: Def1
	= p:pat ':' d:<isLower>+	{ (p, Simple $ mkName d) }
	/ p:pat ':' '(' _:spaces dr:defRslt _:spaces ')' l:listify
					{ (p, DefRslt l dr) }

listify :: Listify
	= '*'				{ List }
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
	/ '[' ']'			{ ConE $ mkName "[]" }

|]
