{-# LANGUAGE OverloadedStrings #-}
module Check.Solve where

import Prelude()
import Check.Types
import qualified Check.Native as Native
import qualified Parse.AST as AST
import Parse.AST (Statement, Expression, Subroutine, Program)
import Parse.Subroutine
import System.Directory
import Data.Graph
import Data.Maybe
import Debug.Trace
import ClassyPrelude

addSubroutine :: State -> Subroutine -> State
addSubroutine state (AST.Native name) = addType ret (Native.native ret) $ foldl' addArg state args
    where ret = Return name
          args = map (Argument name) [0..Native.arity name - 1]
          addArg st a = addType a (Native.native a) st
addSubroutine state (AST.Subroutine name args statement) 
    | not $ hasReturn statement = addType (Return name) Unit withStatement
    | otherwise                 = withStatement
    where ret = Return name
          addArg st (a, i) = introducePoint (Argument name i) $ introducePoint (Variable name a) st
          signature = introducePoint ret $ foldl' addArg state $ zip args [0..]
          withStatement = addStatement name signature statement

hasReturn :: Statement -> Bool
hasReturn (AST.If _ th el)  = hasReturn th || any hasReturn el 
hasReturn (AST.While _ st)  = hasReturn st
hasReturn (AST.Return _)    = True
hasReturn (AST.Routine sts) = any hasReturn sts
hasReturn _                 = False

addStatement :: Text -> State -> Statement -> State
addStatement name state stmt = case stmt of
    AST.Assignment var e -> introducePoint (Variable name var) state
    AST.If _ th el       -> let newState = addStatement name state th in case el of
        Just st -> addStatement name newState st
        Nothing -> newState
    AST.While _ st       -> addStatement name state st
    AST.Routine sts      -> foldl' (addStatement name) state sts
    _                    -> state

getPoint :: Text -> AST.Expression -> TypePoint
getPoint _    (AST.Number _)        = Literal Number
getPoint _    (AST.String _)        = Literal String
getPoint _    (AST.Boolean _)       = Literal Boolean
getPoint name (AST.Variable v)      = Variable name v
getPoint name (AST.Application f es) = case (f, es) of
    ("id", a : _) -> getPoint name a --Redirects to the return type of the argument
    _             -> Return f

connectSubroutine :: State -> Subroutine -> State
connectSubroutine state (AST.Native _)                  = state
connectSubroutine state (AST.Subroutine name args stmt) = step2
    where step1 = connectStatement name state stmt
          step2 = foldl' process step1 (zip args [0..])
          process state (arg, i) = addConnection (Argument name i) (Variable name arg) state

connectStatement :: Text -> State -> Statement -> State
connectStatement name state stmt = case stmt of
    AST.Line expr     -> connectExpression name state expr
    AST.If cond th el -> 
        let step3 = connectStatement name step2 th 
            step2 = connectExpression name step1 cond
            step1 = isBool cond state
        in case el of
            Just ex -> connectStatement name step3 ex
            Nothing -> step3
    AST.While cond s -> 
        let step3 = connectStatement name step2 s
            step2 = connectExpression name step1 cond
            step1 = isBool cond state
        in step3
    AST.Routine ss   -> foldl' (connectStatement name) state ss
    _                -> addConnection this (getPoint name expr) newState
    where (this, expr) = case stmt of
              AST.Assignment s e -> (Variable name s, e)
              AST.Return e       -> (Return name, e)
          newState = case expr of AST.Application _ es -> connectExpression name state expr ; _ -> state
          isBool e = addConnection (getPoint name e) (Literal Boolean)

connectExpression :: Text -> State -> Expression -> State
connectExpression name state expr = case expr of
    AST.Application f es | f `elem` ["eq", "neq"] -> connect es 
                         | otherwise              -> foldl' (process f) state $ zip es [0..]
    _                    -> state
    where process f st (ex, i) = addConnection this (getPoint name ex) newSt where 
              this = Argument f i
              newSt = case ex of AST.Application _ es -> connectExpression name st ex ; _ -> st
          connect [e1, e2] = addConnection (getPoint name e1) (getPoint name e2) state

tryCollect :: [Type] -> Either (Type, Type) (Maybe Type)
tryCollect types = case ordNub $ filter (/= Any) types of
    x : y : _ -> Left (x, y)
    x : _     -> Right $ Just x 
    _         -> Right Nothing

solveSubroutine :: [Subroutine] -> State
solveSubroutine ss = foldl' process raw components
    where raw = foldl' connectSubroutine (foldl' addSubroutine empty ss) ss
          list = getGraph raw
          components = map flattenSCC $ stronglyConnComp $ map (\(x, y) -> (x, x, y)) list
          compType comp = tryCollect $ mapMaybe (`getType` raw) comp
          process state comp = case compType comp of
              Left (t1,t2)     -> reportContradiction comp t1 t2 state
              Right (Just typ) -> foldr (`addType` typ) state comp
              Right Nothing    -> state

base :: (Functor m, MonadIO m) => m State
base = fmap solveSubroutine baseSubroutines

printState :: State -> IO ()
printState state
    | null $ contradictions state = 
        forM_ (points state) $ \pt -> let typ = decide $ getType pt state in case pt of
            Variable f v -> putStrLn $ f ++ "." ++ v ++ " :: " ++ typ
            Return f     -> printF f
            _            -> return ()
    | otherwise                   = mapM_ putStrLn $ contradictions state
    where decide = maybe "Undecidable" tshow
          printF f = putStrLn $ f ++ " :: " ++ intercalate " -> " types
              where types = map decide $ getFunctionType f state

typeCheck :: [Subroutine] -> [Text]
typeCheck = contradictions . solveSubroutine

typeCheckStatement :: (Functor m, MonadIO m) => Statement -> m [Text]
typeCheckStatement st = do
    subs <- baseSubroutines
    return $ typeCheck $ AST.Subroutine "_interactive" [] st : subs

typeCheckStatementWithBase :: [Subroutine] -> Statement -> [Text]
typeCheckStatementWithBase base st = typeCheck $ AST.Subroutine "_interactive" [] st : base