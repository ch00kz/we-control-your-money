module Main where

import Data.Maybe (fromJust)

import Lib
import Accounts

main :: IO ()
main = do 
  maybeAccounts <- getAccounts
  case maybeAccounts of
    Just accounts -> repl accounts
    Nothing -> print "error loading accounts"