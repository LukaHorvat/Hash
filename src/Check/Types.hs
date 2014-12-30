{-# LANGUAGE OverloadedStrings #-}
module Check.Types where

import Prelude ()
import Data.Map (insert, toList)
import Data.Maybe
import Text.Inflections
import ClassyPrelude hiding (toList)

data TypePoint = Variable Text Text 
               | Return Text 
               | Argument Text Int 
               | Literal Type 
               deriving (Show, Ord, Eq)

data Type = Number | Boolean | String | Unit | Any deriving (Show, Ord, Eq)

data State = State 
           { points :: [TypePoint]
           , types :: Map TypePoint Type
           , connections :: Map TypePoint [TypePoint] 
           , contradictions :: [Text] }
           deriving Show
           
empty = State { points = [], connections = mempty, types = mempty, contradictions = [] }

addType :: TypePoint -> Type -> State -> State
addType point typ state 
    | prev `elem` [Nothing, Just Any] = newState { types = insert point typ $ types newState }
    | prev == Just typ                = newState
    | otherwise                       = let Just p = prev in reportContradiction [point] p typ newState
    where newState = introducePoint point state
          prev = getType point newState

getType :: TypePoint -> State -> Maybe Type
getType (Literal t) = const $ Just t
getType point = lookup point . types

introducePoint :: TypePoint -> State -> State
introducePoint point state = state { points = ordNub $ point : points state }

getConnections :: TypePoint -> State -> [TypePoint]
getConnections point state = fromMaybe [] (lookup point $ connections state)

addConnection :: TypePoint -> TypePoint -> State -> State
addConnection p1 p2 state 
    | Just Any `elem` [getType p1 state, getType p2 state] = state --Can't connect to a point that's any. Special condition
    | otherwise = state { connections = insert p1 c1 $ insert p2 c2 $ connections state } 
    where c1 = ordNub $ p2 : getConnections p1 state
          c2 = ordNub $ p1 : getConnections p2 state


getGraph :: State -> [(TypePoint, [TypePoint])]
getGraph = toList . connections

reportContradiction :: [TypePoint] -> Type -> Type -> State -> State
reportContradiction points type1 type2 state = state { contradictions =
    (intercalate ", " (map showPt points) ++ " should be a " ++ tshow type1 ++ ", but also a " ++ tshow type2) : contradictions state }
    where showPt (Argument f n)   = pack (ordinalize (fromIntegral n)) ++ " argument of " ++ f
          showPt (Return f)       = "the return value of " ++ f
          showPt (Variable f var) = "the local variable " ++ var ++ " of the function " ++ f
          showPt (Literal t)      = "the type " ++ tshow t

arity :: Text -> State -> Int
arity f st = case (toMinLen ls :: Maybe (MinLen (Succ Zero) [Int])) of
    Nothing -> 0
    Just xs -> last xs + 1
    where ls = takeWhile ((`elem` points st) . Argument f) [0..] 

getFunctionType :: Text  -> State -> [Maybe Type]
getFunctionType f state = map (flip getType state . Argument f) [0..arity f state - 1] ++ [getType (Return f) state]