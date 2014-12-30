{-# LANGUAGE OverloadedStrings #-}
module Parse.Subroutine where

import Prelude ()
import Data.Ratio
import Data.Maybe
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.Text
import Text.Parsec.Expr
import Text.Parsec.Prim (try)
import Text.Parsec (parse, ParseError)
import Parse.AST
import Parse.Expression
import Parse.Statement
import Parse.Desugar
import Control.Applicative
import System.FilePath (takeBaseName)
import System.Directory
import ClassyPrelude

subroutine :: Text -> Parser Subroutine
subroutine name = (word "native" *> return (Native name))
              <|> (Subroutine name <$> many (withSpaces variable) <*> withSpaces statement)

subroutineFromFile :: (Functor m, MonadIO m) => FilePath -> m (Either ParseError Subroutine)
subroutineFromFile path = 
    parse (subroutine $ pack $ takeBaseName $ fpToString path) "" . desugarIndentation <$> readFile path

baseSubroutines :: (Functor m, MonadIO m) => m [Subroutine]
baseSubroutines = do
    nFiles <- map fpFromString <$> liftIO (getDirectoryContents "base/native")
    nEithers <- mapM (subroutineFromFile . ("base/native/" ++)) (filter (`notElem` [".", ".."]) nFiles)
    bFiles <- map fpFromString <$> liftIO (getDirectoryContents "base")
    bEithers <- mapM (subroutineFromFile . ("base/" ++)) (filter (`notElem` [".", "..", "native"]) bFiles)
    let eithers = nEithers ++ bEithers
    let res = sequence eithers
    case res of
        Left err   -> print err >> return []
        Right subs -> return subs