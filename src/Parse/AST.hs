{-# LANGUAGE DeriveGeneric #-}
module Parse.AST where

import Prelude ()
import Text.PrettyPrint.GenericPretty
import ClassyPrelude

data Expression = Number Rational
                | String Text
                | Boolean Bool
                | Variable Text
                | Binary Text Expression Expression
                | Unary Text Expression
                | Application Text [Expression]
                deriving (Show, Generic)

data Statement = Assignment Text Expression
               | Line Expression
               | If Expression Statement (Maybe Statement)
               | While Expression Statement
               | Return Expression
               | Routine [Statement]
               deriving (Show, Generic)

data Subroutine = Subroutine Text [Text] Statement 
                | Native Text 
                deriving (Show, Generic)

data Program = Program [Subroutine] Statement deriving (Show, Generic)

{-
instance Out Expression
instance Out Statement
instance Out Subroutine
instance Out Program
-}