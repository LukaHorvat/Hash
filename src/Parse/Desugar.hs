{-# LANGUAGE OverloadedStrings #-}
module Parse.Desugar where

import Prelude ()
import Control.Arrow
import Parse.AST
import ClassyPrelude hiding (LT, GT, EQ)

desugarIndentation :: Text -> Text
desugarIndentation src = merge $ pairMap bracketize (("", 0) : (indentPairs ++ [("", 0)]))
    where lines' = lines $ unpack src
          removeIndent = dropWhile (== ' ')
          indentPairs = map (removeIndent &&& ((`div` 4) . length . takeWhile (== ' '))) lines'
          bracketize (_, prev) (line, curr)
              | prev < curr = replicate (curr - prev) '{' ++ line
              | curr < prev = replicate (prev - curr) '}' ++ line
              | otherwise   = "; " ++ line
          pairMap f (x : y : xs) = f x y : pairMap f (y : xs)
          pairMap _ _            = []
          merge = pack . dropWhile (`elem` (" ;" :: String)) . unwords 

desugarInfix :: Statement -> Statement
desugarInfix stat = case stat of
    Assignment s e -> Assignment s $ expr e
    Line e         -> Line $ expr e
    If e th el     -> If (expr e) (desugarInfix th) (fmap desugarInfix el)
    While e s      -> While (expr e) (desugarInfix s)
    Return e       -> Return $ expr e
    Routine ss     -> Routine $ map desugarInfix ss
    where expr (Binary "pipe" e1 e2) = case expr e2 of
              Application n as -> Application n $ as ++ [expr e1]
              _                -> error "Cannot pipe into something that isn't a function"
          expr (Binary "in" e1 e2) = case expr e1 of
              Application n as -> Application n $ as ++ [Application "readFile" [expr e2]]
              _                -> error "Cannot stream into something that isn't a function"
          expr (Binary op l r)    = Application op [expr l, expr r]
          expr (Application s es) = Application s $ map expr es 
          expr (Unary op e)       = Unary op $ expr e
          expr x                  = x

