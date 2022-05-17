{-# LANGUAGE RecordWildCards #-}
module Accounts where

import Data.Maybe (fromJust)
import Data.List (findIndex, find)
import Text.CSV
import Transaction (Transaction(..), Entity(..), Dollar(..))

data Account = 
  Account { accountOwner :: Entity
          , accountBalance :: Dollar
          }
instance Show Account where
  show account = owner ++ " | " ++ balance ++ "\n"
    where
      owner = show $ accountOwner account
      balance = show $ accountBalance account

instance Eq Account where
  (==) a1 a2 = accountOwner a1 == accountOwner a2

-- using intructions here
-- https://subscription.packtpub.com/book/big_data_/9781783286331/1/ch01lvl1sec12/keeping-and-representing-data-from-a-csv-file

{- CSV looks like:
[
  ["andrew","1000","personal"],
  ["max","3000","personal"],
  ["immad","3000","personal"],
  ["mitchell","1500","personal"],
  ["mercury","100000","business"],
  ["Canada","1000000000","government"]
]
-}

type AccountFunction = (Dollar -> Account -> Account) 

creditAccount :: AccountFunction
creditAccount creditAmount account = 
  Account { accountOwner = accountOwner account
          , accountBalance = accountBalance account + creditAmount 
          }

debitAccount :: AccountFunction
debitAccount debitAmount account = 
  Account { accountOwner = accountOwner account
          , accountBalance = accountBalance account - debitAmount 
          }

updateAccount :: AccountFunction -> Entity -> Dollar -> [Account] -> [Account]
updateAccount f owner amount accounts = 
  (take i accounts) ++ [updatedAccount] ++ (drop (i + 1) accounts)
    where
      isAccountOwner o a = accountOwner a == o
      account = fromJust $ find (isAccountOwner owner) accounts -- fromJust will explode with a Nothing
      isAccount = (==) account
      i = fromJust $ findIndex isAccount accounts -- fromJust will explode with a Nothing
      updatedAccount = f amount account

getAccounts :: IO (Maybe [Account])
getAccounts = do
  accountsCSV <- parseCSVFromFile "src/accounts.csv" -- IO actions i a value wrapped in
  case accountsCSV of 
    Right csvData -> pure $ Just $ map (toAccount . listToTuple) csvData
    Left _ -> pure Nothing
  where
    listToTuple [name, balance, kind] = (name, balance, kind) 
    listToTuple _ = error "unexpected row format in accounts CSV"


toAccount :: (String, String, String) -> Account
toAccount (name, balance, kind) = Account {..}
  where
    balanceFromCsv = read balance :: Double
    -- record fields
    accountOwner = case kind of 
      "personal" -> Person name
      "business" -> Business name
      "government" -> Government
      _ -> Person name -- Maybe is probably the right thing but this is "good enough"
    accountBalance = Dollar balanceFromCsv

processTransactions :: [Account] -> [Transaction] -> [Account]
processTransactions accounts [] = accounts
processTransactions accounts (t:ts) = processTransactions (processTransaction accounts t) ts

processTransaction :: [Account] -> Transaction -> [Account]
processTransaction accounts t = (creditToAccount . debitFromAccount) accounts
  where
    fromEntity = txnFrom t
    toEntity = txnTo t
    amount = txnAmount t
    creditToAccount = updateAccount creditAccount toEntity amount
    debitFromAccount = updateAccount debitAccount fromEntity amount