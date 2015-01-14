{-# LANGUAGE OverloadedStrings, LambdaCase #-}
module Interpret.Native where

import Prelude ()
import Data.List (inits)
import Interpret.State (Execution, Value(Unit, Boolean, Number, String), ignore, fromValue, toValue)
import qualified Interpret.State as St
import Numeric
import System.Directory
import System.FilePath hiding (FilePath)
import ClassyPrelude

native :: Text -> [Value] -> Execution Value
native fun vals = do
    let value = fromMaybe (error $ unpack $ "Function " ++ fun ++ " needs more arguments")
    let x = value $ listToMaybe vals
    let y = value $ listToMaybe $ drop 1 vals
    let z = value $ listToMaybe $ drop 2 vals
    let with f = return $ St.with f x y
    case fun of
        "lt" -> with (<)
        "gt" -> with (>)
        "eq" -> with (==)
        "cd" -> cd $ fromValue x
        "id" -> return x
        "or" -> with $ \(Boolean b1) (Boolean b2) -> b1 || b2
        "and" -> with $ \(Boolean b1) (Boolean b2) -> b1 && b2
        "add" -> with (+)
        "sub" -> with (-)
        "div" -> with (/)
        "neq" -> with (/=)
        "leq" -> with (<=)
        "geq" -> with (>=)
        "not" -> let Boolean b = x in return $ Boolean $ not b
        "neg" -> return $ negate x
        "pwd" -> liftIO $ String <$> pack <$> getCurrentDirectory
        "out" -> let String s = y in St.ignore $ writeFile (fpFromText s) $ tshow x
        "app" -> let String s = y in St.ignore $ appendFile (fpFromText s) $ tshow x
        "mult" -> with (*)
        "conc" -> with $ \(String s1) (String s2) -> s1 ++ s2
        "show" -> return $ String $ tshow x
        "print"  -> print x >> return Unit
        "lsCur"  -> liftIO $ liftM (String . pack . unwords . filter (`notElem` ["..", "."])) (getCurrentDirectory >>= getDirectoryContents)
        "readS"  -> String <$> getLine
        "parseN" -> let String s = x in return $ Number $ readRatio $ (unpack :: Text -> String) s
        "parseB" -> let String s = x in return $ Boolean $ fromMaybe (error "Couldn't parse a Boolean") $ (readMay :: Text -> Maybe Bool) s
        "take"   -> return $ toValue $ take (toInt x) (fromValue y :: Text)
        "drop"   -> return $ toValue $ drop (toInt x) (fromValue y :: Text)
        "copyFile"   -> ignoreLift $ copyFile (toString x) (toString y)
        "removeFile" -> ignoreLift $ removeFile $ toString x
        "readFile"   -> let String s = x in String <$> readFile (fpFromText s)
        "fileExists" -> liftIO $ toValue <$> doesFileExist (toString x)
        "dirExists"  -> liftIO $ toValue <$> doesDirectoryExist (toString x)
        "renameDir"  -> ignoreLift $ renameDirectory (toString x) (toString y)
        "combine"    -> return $ valFromString $ combine (toString x) (toString y)
        "fileName"   -> return $ valFromString $ takeFileName $ toString x
        "createDir"  -> ignoreLift $ createDirectory $ toString x
        "baseDir"    -> return $ valFromString $ takeDirectory $ toString x
        "makePath"   -> ignoreLift $ makePath $ toString x
        "removeDir"  -> ignoreLift $ removeDirectoryRecursive $ toString x
        _            -> error $ unpack $ "The function " ++ fun ++ " is not a native function"

toInt :: Value -> Int
toInt v = floor (fromValue v :: Rational)

toString :: Value -> String
toString = unpack . (fromValue :: Value -> Text)

valFromString :: String -> Value
valFromString s = toValue (pack s :: Text)

readRatio :: RealFrac a => String -> a
readRatio s = case readSigned readFloat s of
    (x, _) : _ -> x
    _          -> error "Couldn't parse a Number"

appendFile :: MonadIO m => FilePath -> Text -> m ()
appendFile fp x = do
    file <- readFile fp
    length file `seq` writeFile fp $ file ++ x

ignoreLift :: IO a -> Execution Value
ignoreLift = ignore . liftIO

cd :: Text -> Execution Value
cd ".." = ignoreLift $ join $ setCurrentDirectory <$> takeDirectory <$> getCurrentDirectory
cd path
    | isAbsolute unp = ignoreLift $ setCurrentDirectory unp
    | otherwise      = ignoreLift $ (combine <$> getCurrentDirectory <*> return unp) >>= setCurrentDirectory 
    where unp = unpack path

makePath :: String -> IO ()
makePath path = do
    let dirs = drop 1 $ map concat $ inits $ splitPath path
    mapM_ create dirs
    where create p = doesDirectoryExist p >>= \case
              True -> return ()
              _    -> createDirectory p