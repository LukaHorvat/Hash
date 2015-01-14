{-# LANGUAGE OverloadedStrings #-}
module Parse.Statement where

import Prelude ()
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.Text
import Text.Parsec.Prim (try)
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

comment :: Parser Text
comment = pack <$> (char '#' *> many (noneOf ";"))

statement :: Parser Statement
statement = desugarAST <$> (stmt <* optional (symbol ';'))
    where stmt = (routine
              <|> try if'
              <|> try while
              <|> try return'
              <|> try assignment
              <|> line) <* optional comment

statements :: Parser Statement
statements = Routine <$> many (withSpaces statement)