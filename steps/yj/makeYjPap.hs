{-# LANGUAGE QuasiQuotes, TypeFamilies #-}

import ParseYjPap

main :: IO ()
main = do
	str <- getContents
	either putStrLn print $ parseYjPap str
