{-# LANGUAGE OverloadedStrings #-}
module Parse.Statement where

import Prelude ()
import Data.Ratio
import Data.Maybe
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.Text
import Text.Parsec.Expr
import Text.Parsec.Prim (try)
import Control.Monad.Identity
import Parse.AST
import Parse.Expression
import Parse.Desugar
import Control.Applicative hiding (optional)
import ClassyPrelude hiding (try)

assignment :: Parser Statement
assignment = Assignment 
         <$> (withSpaces variable <* symbol '=') 
         <*> withSpaces expression

line :: Parser Statement
line = Line <$> withSpaces expression

return' :: Parser Statement
return' = Return <$> (withSpaces (string "return") *> withSpaces expression)

routine :: Parser Statement
routine = symbol '{' *> statements <* symbol '}' 

if' :: Parser Statement
if' = If 
  <$> (withSpaces (string "if") *> withSpaces expression)
  <*> withSpaces statement
  <*> optionMaybe (try else')
  where else' = withSpaces (string "else") *> withSpaces statement

while :: Parser Statement
while = While
    <$> (withSpaces (string "while") *> withSpaces expression)
    <*> withSpaces statement

pipe :: Parser Statement
pipe = do
    expr <- withSpaces expression 
    sep <- try (word "|>>") <|> word "|>" <|> word "<|"
    str <- stringLit
    let p = Pipe $ case sep of
            "|>>" -> Append
            "|>"  -> Out
            "<|"  -> In
    return $ p expr str

comment :: Parser Text
comment = pack <$> (char '#' *> many anyChar)

statement :: Parser Statement
statement = desugarInfix <$> (stmt <* optional (symbol ';'))
    where stmt = (routine
              <|> try if'
              <|> try while
              <|> try return'
              <|> try assignment
              <|> try pipe
              <|> line) <* optional comment

statements :: Parser Statement
statements = Routine <$> many (withSpaces statement)