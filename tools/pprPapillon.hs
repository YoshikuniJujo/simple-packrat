import Text.Papillon
import System.Environment
import Language.Haskell.TH
import Language.Haskell.TH.Quote

main :: IO ()
main = do
	fp : _ <- getArgs
	cnt <- readFile fp
	print . ppr =<< runQ (quoteDec papillon cnt)
