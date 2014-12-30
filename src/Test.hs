module Test where

import Parse.Statement
import Parse.Desugar
import Text.Parsec
import Parse.AST
import Text.PrettyPrint.GenericPretty

parseFile :: String -> IO Statement
parseFile path = do
    file <- readFile path
    let Right s = parse statement "" $ desugarIndentation file
    return s

prettyFile :: String -> IO ()
prettyFile path = do
    s <- parseFile path
    pp s