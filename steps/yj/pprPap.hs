import System.Environment
import Language.Haskell.TH
import Language.Haskell.TH.Quote

import MakeYjPap

main :: IO ()
main = do
	fp : _ <- getArgs
	print . ppr =<< runQ . quoteDec pap =<< readFile fp
