import MkYjMemo

import Language.Haskell.TH

main :: IO ()
main = do
	runQ decs >>= print . ppr
