module Transaction where

{-what the CSV will look like
  andrew,500,individual
  government,9000000000,government
  google,1000000,business
-}

data Entity = Business String | Person String | Government deriving (Show)

newtype Dollar = Dollar {unDollar :: Double}
instance Show Dollar where
  show dollar = "$" ++ show (unDollar dollar)

data TransactionType = Transfer | Taxation | Refund | Salary deriving (Show)

data Transaction = 
  Txn { txnType :: TransactionType -- type is a reserved word?
      , txnAmount  :: Dollar 
      , txnTo      :: Entity 
      , txnFrom    :: Entity } 
  deriving Show
