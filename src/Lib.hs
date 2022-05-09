module Lib
  ( repl
  ) where

import Commands
 
-- consider forever https://hackage.haskell.org/package/base-4.16.1.0/docs/Control-Monad.html#v:forever

repl :: IO ()
repl = do
  putStrLn "Enter a command:"
  cmdStr <- getLine
  case parseCommand cmdStr of
    Just ExitCommand -> putStrLn "That's all folks"
    Nothing -> do
      putStrLn "unknown command" 
      repl
    Just cmd -> do
      print $ handleCommand cmd 
      repl

