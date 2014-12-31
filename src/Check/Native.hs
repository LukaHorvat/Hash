{-# LANGUAGE OverloadedStrings #-}
module Check.Native where

import Prelude ()
import Check.Types
import ClassyPrelude

native :: TypePoint -> Type
native point = case point of
    Return   s   -> snd $ typ s
    Argument s n -> 
        fromMaybe (error $ unpack $ "The function " ++ s ++ " doesn't have " ++ tshow (n + 1) ++ " arguments") (maybeType s n)
    where maybeType :: Text -> Int -> Maybe Type
          maybeType s n = listToMaybe $ drop n $ fst $ typ s

arity :: Text -> Int
arity = length . fst . typ

typ "print"  = ([Any], Unit)
typ "add"    = ([Number, Number], Number)
typ "sub"    = ([Number, Number], Number)
typ "mult"   = ([Number, Number], Number)
typ "div"    = ([Number, Number], Number)
typ "lt"     = ([Number, Number], Boolean)
typ "gt"     = ([Number, Number], Boolean)
typ "leq"    = ([Number, Number], Boolean)
typ "geq"    = ([Number, Number], Boolean)
typ "eq"     = ([Any, Any], Boolean)
typ "neq"    = ([Any, Any], Boolean)
typ "and"    = ([Boolean, Boolean], Boolean)
typ "or"     = ([Boolean, Boolean], Boolean)
typ "neg"    = ([Number], Number)
typ "not"    = ([Boolean], Boolean)
typ "readS"  = ([], String)
typ "parseN" = ([String], Number)
typ "parseB" = ([String], Boolean)
typ "pwd"    = ([], String)
typ "cd"     = ([String], Unit)
typ "in"     = ([Any, String], Any)
typ "out"    = ([Any, String], Unit)
typ "app"    = ([Any, String], Unit)
typ "pipe"   = ([Any, Any], Any)
typ "id"     = ([Any], Any)
typ "readFile" = ([String], String)