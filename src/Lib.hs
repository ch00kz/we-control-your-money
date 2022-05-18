module Lib
  ( repl
  ) where

import Commands
import Accounts (Account, processTransactions)
 
-- consider forever https://hackage.haskell.org/package/base-4.16.1.0/docs/Control-Monad.html#v:forever

repl :: [Account] -> IO ()
repl accounts = do
  putStrLn "Enter a command:"
  cmdStr <- getLine
  case parseCommand cmdStr of
    Just ExitCommand -> putStrLn "That's all folks"
    Nothing -> do
      putStrLn "unknown command" 
      repl accounts
    Just ShowAccountsCommand -> do 
      print accounts
      repl accounts
    Just cmd -> do
      let transacations = handleCommand cmd
      print transacations 
      updatedAccounts <- processTransactions accounts transacations
      repl updatedAccounts