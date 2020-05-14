module Main where

import Relude
import Text.Pandoc.Url2Cite (url2cite)
import Text.Pandoc.JSON

main :: IO ()
main = toJSONFilter url2cite
