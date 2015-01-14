{-# LANGUAGE OverloadedStrings, ViewPatterns #-}
module Main where

import Prelude ()
import Interpret.Eval
import Parse.Subroutine
import System.Directory
import System.FilePath hiding (FilePath)
import ClassyPrelude hiding ((</>))

findBasePath :: [Text] -> IO Text
findBasePath ("--base" : p : xs) = return p
findBasePath (p : xs)            = findBasePath xs
findBasePath _                   = pack <$> (</> "hash") <$> getHomeDirectory

findFilePath :: [Text] -> Text
findFilePath ((take 2 -> "--") : _ : xs) = findFilePath xs
findFilePath (x : xs)                    = x
findFilePath _                           = ""

main :: IO ()
main = do
    args <- getArgs
    cwd <- pack <$> getCurrentDirectory :: IO Text
    baseDir <- findBasePath args
    setCurrentDirectory $ unpack baseDir
    env <- base
    baseSubs <- baseSubroutines
    setCurrentDirectory $ unpack cwd
    if null $ findFilePath args then interactiveWithBase env baseSubs
    else do
        let filePath = fpFromText $ findFilePath args
        evalFile filePath env baseSubs