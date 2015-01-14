{-# LANGUAGE OverloadedStrings #-}
module Parse.Subroutine where

import Prelude ()
import Text.Parsec.Text
import Text.Parsec (parse, ParseError, char)
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
              <|> (Subroutine name <$> many (withSpaces (char ':' *> identifier)) <*> (toStmt <$> many (withSpaces statement)))
              where toStmt [x] = x
                    toStmt xs  = Routine xs

subroutineFromFile :: (Functor m, MonadIO m) => FilePath -> m (Either ParseError Subroutine)
subroutineFromFile path = 
    parse (subroutine $ pack $ takeBaseName $ fpToString path) "" . desugarString <$> readFile path

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