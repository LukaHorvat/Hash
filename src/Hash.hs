module Hash (runFromFileWithBase, runFromFile, runInteractiveWithBase, runInteractive) where

import Prelude ()
import Interpret.Eval
import Parse.Subroutine
import Parse.AST
import Interpret.State
import System.Directory
import System.FilePath hiding (FilePath)
import ClassyPrelude hiding ((</>))

getBase :: IO (Environment, [Subroutine])
getBase = do
    baseDir <- (</> "hash") <$> getHomeDirectory
    cwd <- getCurrentDirectory
    setCurrentDirectory baseDir
    env <- base
    baseSubs <- baseSubroutines
    setCurrentDirectory cwd
    return (env, baseSubs)


runFromFileWithBase :: Environment -> [Subroutine] -> FilePath -> IO ()
runFromFileWithBase env baseSubs path = evalFile path env baseSubs

runFromFile :: FilePath -> IO ()
runFromFile path = do
    (env, baseSubs) <- getBase
    runFromFileWithBase env baseSubs path

runInteractiveWithBase :: Environment -> [Subroutine] -> IO ()
runInteractiveWithBase = interactiveWithBase

runInteractive = do
    (env, baseSubs) <- getBase
    runInteractiveWithBase env baseSubs

