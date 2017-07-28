{-# LANGUAGE TemplateHaskell #-}

import System.Environment

import MkYjMemo

decs

main :: IO ()
main = print . (fst <$>) . yj . parse . head =<< getArgs

data Initial = Y | J deriving Show
