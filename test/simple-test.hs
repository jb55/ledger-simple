{-# LANGUAGE OverloadedStrings #-}

module Main where

import Text.Ledger.Simple
import Control.Monad (unless)
import System.IO (stderr, hPutStr)
import System.Exit (exitFailure)
import Data.Time.Calendar

main :: IO ()
main = test_simple

printerr :: String -> IO ()
printerr = hPutStr stderr

assertEq :: (Show a, Eq a) => String -> a -> a -> IO ()
assertEq name got expected = unless (got == expected) $
  do printerr (name ++ " failed\n")
     printerr "got:\n\n"
     printerr (show got)
     printerr "\n\nexpected:\n\n"
     printerr (show expected)
     printerr "\n"
     exitFailure


test_simple :: IO ()
test_simple = assertEq "test_simple" (renderTransaction tx) expected
  where
    posting  = Posting "125 CAD" "assets:bank" (Just "xfr")
    tx       = Transaction day "Test transaction" (pure posting)
    day      = fromGregorian 2017 9 14
    expected = "2017/9/14 Test transaction\n    assets:bank\t125 CAD ;xfr\n\n"
