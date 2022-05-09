{-# LANGUAGE RecordWildCards #-}
module Commands where

import Text.Regex.TDFA
import Transaction

data Command = 
    TaxCommand { cmdAmount :: Dollar, cmdFrom :: Entity }
  | RefundCommand { cmdAmount :: Dollar, cmdTo :: Entity }
  | PaymentCommand 
      { cmdAmount :: Dollar
      , cmdTo:: Entity
      , cmdFrom :: Entity 
      }
  | TransferCommand 
      { cmdAmount :: Dollar
      , cmdTo :: Entity
      , cmdFrom :: Entity 
      }
  | ExitCommand
  deriving Show

parseCommand :: String -> Maybe Command
parseCommand cmdStr
  | isExitCommand cmdStr = Just ExitCommand
  | isTaxCommand cmdStr = Just $ TaxCommand 
      { cmdAmount = Dollar $ getPart read 2
      , cmdFrom = getPart Person 1 
      }
  | isRefundCommand cmdStr = Just $ RefundCommand 
      { cmdAmount = Dollar $ getPart read 2
      , cmdTo = getPart Person 1 
      }  
  | isPaymentCommand cmdStr = Just $ PaymentCommand 
      { cmdAmount = Dollar $ getPart read 3
      , cmdTo = getPart Person 2
      , cmdFrom = getPart Business 0
    }
  | isTransferCommand cmdStr = Just $ TransferCommand 
    { cmdAmount = Dollar $ getPart read 3
    , cmdTo= getPart Person 2
    , cmdFrom = getPart Business 0
  }
  | otherwise = Nothing
  where
    parts = words cmdStr
    getPart f x = f $ parts !! x

handleCommand :: Command -> [Transaction]
handleCommand cmd = 
  case cmd of 
    TaxCommand {..} -> 
      [ Txn 
          { txnType = Taxation
          , txnAmount = cmdAmount
          , txnTo = Government
          , txnFrom = cmdFrom
          }
      ]
    RefundCommand {..} -> 
      [ Txn 
          { txnType = Refund
          , txnAmount = cmdAmount
          , txnTo = cmdTo
          , txnFrom = Government
          }
      ]
    PaymentCommand {..} ->
      [
        Txn 
          { txnType = Salary
          , txnAmount = cmdAmount
          , txnTo = cmdTo
          , txnFrom = cmdFrom
          }
      ,
        Txn 
          { txnType = Taxation
          , txnAmount = Dollar $ (unDollar cmdAmount) * 0.15
          , txnTo = Government
          , txnFrom = cmdFrom
          }
      ]
    TransferCommand {..} ->
      [
        Txn 
          { txnType = Transfer
          , txnAmount = cmdAmount
          , txnTo = cmdTo
          , txnFrom = cmdFrom
          }
      ]

-- exit
isExitCommand :: String -> Bool
isExitCommand cmd = cmd == "exit"

-- <business> pays <person> <amount>
isPaymentCommand :: String -> Bool
isPaymentCommand cmd = cmd =~ "([[A-Za-z]+) pays ([[A-Za-z]+) ([0-9]+)" :: Bool

-- tax <person> <amount>
isTaxCommand :: String -> Bool
isTaxCommand cmd = cmd =~ "tax ([[A-Za-z]+) ([0-9]+)" :: Bool

isRefundCommand :: String -> Bool
isRefundCommand cmd = cmd =~ "refund ([[A-Za-z]+) ([0-9]+)" :: Bool

-- transfer <amount> from <person> to <person>
isTransferCommand :: String -> Bool
isTransferCommand cmd = cmd =~ "transfer ([0-9]+) from ([[A-Za-z]+) to ([[A-Za-z]+)" :: Bool