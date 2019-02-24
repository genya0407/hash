{-# LANGUAGE ScopedTypeVariables #-}
module Hash.BuiltinCommand(builtinCommands) where

import System.Exit (ExitCode(..), exitWith)
import System.Directory (setCurrentDirectory, getCurrentDirectory)
import Control.Exception (catch, IOException)
import Data.List (intercalate)
import qualified Data.HashMap.Strict as H

type BuiltinCommand = [String] -> IO ExitCode

builtinCommands :: H.HashMap String BuiltinCommand
builtinCommands = H.fromList [("cd", cd)]

cd :: BuiltinCommand
cd [] = cd ["~"]
cd (dir:[]) = do
    let changedir = setCurrentDirectory dir >> return ExitSuccess
    let whenError = putStrLn "Error!" >> (return $ ExitFailure 1)
    catch changedir $ \(e :: IOException) -> whenError
cd args = do
    putStrLn $ "wrong arguments: " ++ intercalate " " args
    return $ ExitFailure 1
