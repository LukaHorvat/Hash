{-# LANGUAGE OverloadedStrings #-}
module Parse.Expression where

import Numeric
import Data.Ratio
import Data.Char
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.Text
import Text.Parsec.Expr
import Text.Parsec.Prim (try)
import Control.Monad.Identity
import Parse.AST
import Control.Applicative
import Data.Text (Text)
import Prelude ()
import ClassyPrelude hiding (LT, GT, EQ, try)
import Parse.Common

(<:>) :: Applicative f => f a -> f [a] -> f [a]
(<:>) = liftA2 (:)

withSpaces :: Parser a -> Parser a
withSpaces p = p <* many space

withSpaces1 :: Parser a -> Parser a
withSpaces1 p = p <* many1 space

symbol :: Char -> Parser Char
symbol = withSpaces . char

word :: String -> Parser Text
word = withSpaces . text

operator :: String -> Parser String
operator str = withSpaces $ string str <* notFollowedBy (satisfy isSymbol)

number :: Parser Rational
number = do
    m <- optionMaybe (string "-")
    num <- many1 digit
    dec <- optionMaybe (char '.' <:> many1 digit) 
    return $ readRatio $ concat $ catMaybes [m, Just num, dec]
    where readRatio s = let (x, _) : _ = readSigned readFloat s in x

boolean :: Parser Bool
boolean = (text "True" *> return True) <|> (text "False" *> return False)

stringLit :: Parser Text
stringLit = do
    char '"'
    str <- pack <$> many allowed
    char '"'
    return str
    where allowed = text "\\\\" *> return '\\'
                <|> text "\\n"  *> return '\n'
                <|> text "\\t"  *> return '\t'
                <|> text "\\r"  *> return '\r'
                <|> text "\\\"" *> return '"'
                <|> noneOf "\""

identifier :: Parser Text
identifier = pack <$> letter <:> many alphaNum

variable :: Parser Text
variable = char '_' *> identifier

application :: Parser Expression
application = Application <$> withSpaces identifier <*> many term

table :: OperatorTable Text () Identity Expression
table = map (map ((`Infix` AssocLeft) . try)) [[mult, div], [add, sub], [lt, gt, eq, neq, leq, geq], [and'], [or'], [in', out, app]]
    where add  = operator "+"  *> return (Binary "add")
          sub  = operator "-"  *> return (Binary "sub")
          mult = operator "*"  *> return (Binary "mult")
          div  = operator "/"  *> return (Binary "div")
          lt   = operator "<"  *> return (Binary "lt")
          gt   = operator ">"  *> return (Binary "gt")
          eq   = operator "==" *> return (Binary "eq")
          neq  = operator "!=" *> return (Binary "neq")
          leq  = operator "<=" *> return (Binary "leq")
          geq  = operator ">=" *> return (Binary "geq")
          and' = operator "&"  *> return (Binary "and")
          or'  = operator "|"  *> return (Binary "or")
          in'  = operator "<|" *> return (Binary "in")
          out  = operator "|>" *> return (Binary "out")
          app  = operator "|>>" *> return (Binary "app")

term :: Parser Expression          
term = (symbol '(' *> expression <* symbol ')') <|> leaf
    where leaf = fmap Number   (withSpaces number) 
             <|> fmap String   (withSpaces stringLit) 
             <|> fmap Variable (withSpaces variable) 
             <|> fmap Boolean  (withSpaces boolean)
             <|> withSpaces application

expression :: Parser Expression
expression = buildExpressionParser table term