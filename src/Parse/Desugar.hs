{-# LANGUAGE OverloadedStrings #-}
module Parse.Desugar where

import Prelude ()
import Control.Arrow
import Parse.AST
import Data.Char
import ClassyPrelude hiding (LT, GT, EQ)

removeIndent :: String -> String
removeIndent = dropWhile (== ' ')

{-
comment :: String -> String
comment ""         = ""
comment ('#' : _)  = ""
comment ('"' : xs) = '"' : stringLit xs
comment (x : xs)   = x : comment xs

stringLit :: String -> String
stringLit ('\\' : '\\' : xs) = "\\\\" ++ stringLit xs
stringLit ('\\' : '"' : xs)  = "\\\"" ++ stringLit xs
stringLit ('"' : xs)         = '"' : comment xs
stringLit _                  = error "String not terminated"
-}

desugarString :: Text -> Text
desugarString src = merge $ pairMap bracketize (("", 0) : (indentPairs ++ [("", 0)]))
    where lines' = filter (any $ flip notElem (" \t" :: String)) $ lines (unpack src :: String)
          indentPairs = map (removeIndent &&& ((`div` 4) . length . takeWhile (== ' '))) lines'
          bracketize (_, prev) (line, curr)
              | prev < curr = replicate (curr - prev) '{' ++ line
              | curr < prev = replicate (prev - curr) '}' ++ line
              | otherwise   = "; " ++ line
          pairMap f (x : y : xs) = f x y : pairMap f (y : xs)
          pairMap _ _            = []
          merge = pack . dropWhile (`elem` (" ;" :: String)) . unwords 

desugarAST :: Statement -> Statement
desugarAST stat = case stat of
    Assignment s e -> Assignment s $ expr e
    Line e         -> Line $ expr e
    If e th el     -> If (expr e) (desugarAST th) (fmap desugarAST el)
    While e s      -> While (expr e) (desugarAST s)
    Return e       -> Return $ expr e
    Routine ss     -> Routine $ map desugarAST ss
    where expr (Binary "pipe" e1 e2) = case expr e2 of
              Application n as -> Application n $ as ++ [expr e1]
              _                -> error "Cannot pipe into something that isn't a function"
          expr (Binary "in" e1 e2) = case expr e1 of
              Application n as -> Application n $ as ++ [Application "readFile" [expr e2]]
              _                -> error "Cannot stream into something that isn't a function"
          expr (Binary op l r)    = Application op [expr l, expr r]
          expr (Application "ls" []) = Application "lsCur" []
          expr (Application s es) = Application s $ map expr es 
          expr (Unary op e)       = Unary op $ expr e
          expr x                  = x