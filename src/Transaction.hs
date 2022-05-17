module Transaction where

{-what the CSV will look like
  andrew,500,individual
  government,9000000000,government
  google,1000000,business
-}

data Entity = 
    Government 
  | Person String 
  | Business String 
  deriving (Eq)

instance Show Entity where
  show (Person name) = name ++ " (Person)"
  show (Business name) = name ++ " (Business)"
  show (Government) = "Government"

-- Dollar 
newtype Dollar = Dollar {unDollar :: Double}

instance Show Dollar where
  show (Dollar amt) = "$" ++ show amt

-- https://stackoverflow.com/questions/34699716/illegal-type-signature-in-instance-declaration
-- You have a type signature in an instance declaration. That's illegal in standard Haskell. 
instance Num Dollar where 
  (+) (Dollar a) (Dollar b) = Dollar (a + b)
  (-) (Dollar a) (Dollar b) = Dollar (a - b)
  (*) _ _ = error "you cant do this with Dollar"
  negate _ = error "you cant do this with Dollar"
  abs _ = error "you cant do this with Dollar"
  signum _ = error "you cant do this with Dollar"
  fromInteger _ = error "you cant do this with Dollar"

data TransactionType = 
    Transfer 
  | Taxation 
  | Refund 
  | Salary 
  deriving (Show)

data Transaction = 
  Txn { txnType :: TransactionType
      , txnAmount  :: Dollar 
      , txnTo      :: Entity 
      , txnFrom    :: Entity } 
  deriving Show
