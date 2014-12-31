{-# LANGUAGE OverloadedStrings #-}
module Interpret.Native where

import Prelude ()
import Interpret.State (Execution, Value(Unit, Boolean, Number, String), ignore)
import qualified Interpret.State as St
import Control.Monad.Trans.Either
import Numeric
import System.Directory
import System.FilePath hiding (FilePath)
import ClassyPrelude

native :: Text -> [Value] -> Execution Value
native fun vals = do
    let value = fromMaybe (error $ unpack $ "Function " ++ fun ++ " needs more arguments")
    let first = value $ listToMaybe vals
    let second = value $ listToMaybe $ drop 1 vals
    let with f = return $ St.with f first second
    case fun of
        "print"  -> print first >> return Unit
        "add"    -> with (+)
        "sub"    -> with (-)
        "mult"   -> with (*)
        "div"    -> with (/)
        "lt"     -> with (<)
        "gt"     -> with (>)
        "eq"     -> with (==)
        "neq"    -> with (/=)
        "leq"    -> with (<=)
        "geq"    -> with (>=)
        "or"     -> with $ \(Boolean b1) (Boolean b2) -> b1 || b2
        "and"    -> with $ \(Boolean b1) (Boolean b2) -> b1 && b2
        "not"    -> let Boolean b = first in return $ Boolean $ not b
        "neg"    -> return $ negate first
        "readS"  -> String <$> getLine
        "parseN" -> let String s = first in return $ Number $ readRatio $ (unpack :: Text -> String) s
        "parseB" -> let String s = first in return $ Boolean $ fromMaybe (error "Couldn't parse a Boolean") $ (readMay :: Text -> Maybe Bool) s
        "pwd"    -> liftIO $ String <$> pack <$> getCurrentDirectory
        "cd"     -> cd (let String s = first in s)
        "out"    -> let String s = second in St.ignore $ writeFile (fpFromText s) $ tshow first
        "app"    -> let String s = second in St.ignore $ appendFile (fpFromText s) $ tshow first
    
readRatio s = case readSigned readFloat s of
    (x, _) : _ -> x
    _          -> error "Couldn't parse a Number"

appendFile :: MonadIO m => FilePath -> Text -> m ()
appendFile fp x = do
    file <- readFile fp
    length file `seq` writeFile fp $ file ++ x

cd :: Text -> Execution Value
cd ".." = ignore $ liftIO $ join $ setCurrentDirectory <$> takeDirectory <$> getCurrentDirectory
