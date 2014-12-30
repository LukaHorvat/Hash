{-# LANGUAGE OverloadedStrings #-}
module Interpret.Native where

import Prelude ()
import Interpret.State (Execution, Value(Unit, Boolean, Number, String), ignore)
import qualified Interpret.State as St
import Control.Monad.Trans.Either
import Numeric
import System.Directory
import System.FilePath
import ClassyPrelude

native :: Text -> [Value] -> Execution Value
native fun vals = do
    let value = fromMaybe (error $ unpack $ "Function " ++ fun ++ " needs more arguments")
    let first = value $ listToMaybe vals
    let second = value $ listToMaybe $ drop 1 vals
    let with f = return $ St.with f first second
    case fun of
        "print" -> print first >> return Unit
        "add"   -> with (+)
        "sub"   -> with (-)
        "mult"  -> with (*)
        "div"   -> with (/)
        "lt"    -> with (<)
        "gt"    -> with (>)
        "eq"    -> with (==)
        "neq"   -> with (/=)
        "leq"   -> with (<=)
        "geq"   -> with (>=)
        "or"    -> with $ \(Boolean b1) (Boolean b2) -> b1 || b2
        "and"   -> with $ \(Boolean b1) (Boolean b2) -> b1 && b2
        "not"   -> let Boolean b = first in return $ Boolean $ not b
        "neg"   -> return $ negate first
        "readS" -> String <$> getLine
        "readN" -> Number . readRatio . (unpack :: Text -> String) <$> getLine
        "readB" -> Boolean . fromMaybe (error "Coulsn't parse a Boolean") . (readMay :: Text -> Maybe Bool) <$> getLine
        "pwd"   -> liftIO $ String <$> pack <$> getCurrentDirectory
        "cd"    -> cd (let String s = first in s)
    where readRatio s = let (x, _) : _ = readSigned readFloat s in x

cd :: Text -> Execution Value
cd ".." = ignore $ liftIO $ join $ setCurrentDirectory <$> takeDirectory <$> getCurrentDirectory
