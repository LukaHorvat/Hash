{-# LANGUAGE ScopedTypeVariables, OverloadedStrings #-}
module Interpret.Eval where

import Prelude ()
import Text.Parsec (parse)
import Parse.Statement hiding (line)
import Parse.Subroutine
import Interpret.State (Value, Environment, Execution)
import qualified Interpret.State as St
import Parse.AST (Expression, Statement, Subroutine)
import qualified Parse.AST as AST
import Interpret.Native
import Control.Monad.State.Class hiding (state)
import Control.Monad.Reader.Class
import Control.Monad.Trans.Either
import Check.Solve (typeCheckStatementWithBase, typeCheck)
import ClassyPrelude

evalExpression :: Expression -> Execution Value
evalExpression (AST.Number n)         = return $ St.Number n
evalExpression (AST.String s)         = return $ St.String s
evalExpression (AST.Boolean b)        = return $ St.Boolean b
evalExpression (AST.Variable n)       = gets (St.readVar n)
evalExpression (AST.Application n es) = do
        f <- asks (St.getFunction n)
        args <- mapM evalExpression es
        f args
evalExpression _ = error "Processing a sugared expression. Need to desugar first"

evalStatement :: Statement -> Execution ()
evalStatement (AST.Assignment n e) = evalExpression e >>= \res -> modify (St.assign n res)
evalStatement (AST.Line e) = evalExpression e >>= defaultPrint
evalStatement (AST.If cond th el) = evalExpression cond >>= \(St.Boolean res) -> 
    if res then evalStatement th
    else mapM_ evalStatement el
evalStatement wh@(AST.While cond st) = evalExpression cond >>= \(St.Boolean res) ->
    if res then evalStatement st >> evalStatement wh
    else return ()
evalStatement (AST.Return e) = evalExpression e >>= left
evalStatement (AST.Routine ss) = mapM_ evalStatement ss

defaultPrint :: MonadIO m => Value -> m ()
defaultPrint v = when (v /= St.Unit) $ print v

subroutineName :: Subroutine -> Text
subroutineName (AST.Subroutine n _ _) = n
subroutineName (AST.Native n)         = n

subroutineFunction :: Subroutine -> [Value] -> Execution Value
subroutineFunction (AST.Native n) args = native n args
subroutineFunction (AST.Subroutine _ args stmt) vals = St.withState state (evalStatement stmt)
    where state = foldr (uncurry St.assign) mempty (zip args vals)

base :: IO Environment
base = foldl' addFunc mempty <$> baseSubroutines
    where addFunc state sub = St.addFunction (subroutineName sub) (subroutineFunction sub) state
    
interactive :: IO ()
interactive = do
    env <- base
    types <- baseSubroutines
    interactiveWithBase env types

interactiveWithBase :: Environment -> [Subroutine] -> IO ()
interactiveWithBase env types =
    void $ St.runExecution env $ forever $ do
        line <- getLine
        let res = parse statements "" line
        case res of
            Left err -> print err
            Right s  -> case typeCheckStatementWithBase types s of
                [] -> evalStatement s
                _  -> putStrLn "Type error"

evalFile :: (Functor m, MonadIO m) => FilePath -> Environment -> [Subroutine] -> m ()
evalFile path baseEnv baseSubs = do
    res <- subroutineFromFile path
    case res of
        Left err  -> print err
        Right sub -> liftIO $ void $ St.runExecution baseEnv $
            case typeCheck (sub : baseSubs) of
                [] -> void $ subroutineFunction sub []
                _  -> putStrLn "Type error"
