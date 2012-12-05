module Main where

import Test.DocTest

main :: IO ()
main = doctest ["Codec/Base64.hs"]
