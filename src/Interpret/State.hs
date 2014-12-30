{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, OverloadedStrings #-}
module Interpret.State where

import Prelude ()
import Data.Map (insert)
import Data.Ratio
import Control.Monad.Trans.Either
import Control.Monad.Trans.State (StateT, runStateT)
import Control.Monad.State.Class
import ClassyPrelude

data Value = String Text 
           | Number Rational
           | Boolean Bool
           | Unit
           deriving Eq

instance Show Value where
    show (String s)  = unpack s
    show (Number n)  = show $ (fromRational :: Rational -> Double) n
    show (Boolean b) = show b
    show Unit        = "()"

instance Num Value where
    (Number x) + (Number y) = Number $ x + y
    negate (Number x) = Number $ -x
    (Number x) * (Number y) = Number $ x * y
    abs (Number x) = Number $ abs x
    signum (Number x) = Number $ signum x
    fromInteger x = Number $ fromInteger x

class Valuable a where
    toValue   :: a -> Value
    fromValue :: Value -> a

instance Valuable Text where
    toValue = String
    fromValue (String s) = s

instance Valuable Rational where
    toValue = Number
    fromValue (Number x) = x

instance Valuable Bool where
    toValue = Boolean
    fromValue (Boolean b) = b

instance Valuable () where
    toValue = const Unit
    fromValue Unit = ()

instance Valuable Value where
    toValue = id
    fromValue = id

instance Fractional Value where
    (Number x) / (Number y) = Number $ x / y
    fromRational = Number

instance Ord Value where
    compare (Number x) (Number y) = compare x y

data Environment = Environment
                 { subroutines :: Map Text ([Value] -> Execution Value) }

data State = State
           { scope :: Map Text Value }
           deriving Show

instance Monoid Environment where
    mempty = Environment { subroutines = mempty }
    l `mappend` r = Environment { subroutines = subroutines l `mappend` subroutines r }

instance Monoid State where
    mempty = State { scope = mempty }
    l `mappend` r = State { scope = scope l `mappend` scope r }

assign :: Text -> Value -> State -> State
assign var val state = state { scope = insert var val (scope state) }

readVar :: Text -> State -> Value
readVar name state = value $ lookup name $ scope state
    where value = fromMaybe (error $ unpack $ "Variable " ++ name ++ " not in scope")

getFunction :: Text -> Environment -> [Value] -> Execution Value
getFunction name = value . lookup name . subroutines
    where value = fromMaybe (error $ unpack $ "The function " ++ name ++ " does not exist")

addFunction :: Text -> ([Value] -> Execution Value) -> Environment -> Environment
addFunction name f env = env { subroutines = insert name f (subroutines env) }

with :: Valuable a => (Value -> Value -> a) -> Value -> Value -> Value
with f l r = toValue $ f l r

type Execution a = EitherT Value (StateT State (ReaderT Environment IO)) a

getReturn :: Execution a -> Execution Value 
getReturn e = lift $ either id (const Unit) <$> runEitherT e

withState :: State -> Execution a -> Execution Value
withState state ex = do
    st <- get
    put state
    res <- getReturn ex
    put st
    return res

runExecution :: Environment -> Execution a -> IO (Value, State)
runExecution env = flip runReaderT env . flip runStateT mempty . (either id (const Unit) <$>) . runEitherT

ignore :: Execution a -> Execution Value
ignore e = e >> return Unit