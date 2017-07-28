import Language.Haskell.TH

main :: IO ()
main = do
	runQ decs >>= print . ppr

decs :: Q [Dec]
decs = do
	return [
		DataD [] (mkName "Derivs") [] Nothing
			[RecC (mkName "Derivs") [
				(mkName "yj", Bang NoSourceUnpackedness NoSourceStrictness, ConT (mkName "Hige"))
				]] []
		]
