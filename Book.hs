module Book where

data Book = Book
  { bookId :: Int
  , title :: String
  , author :: String
  , available :: Bool
  } deriving (Show, Read, Eq)