module Library where

import Book
import Data.List (isInfixOf)
import Data.Char (toLower)

type Library = [Book]

-- Display books
displayBooks :: Library -> IO ()
displayBooks [] = putStrLn "No books available."
displayBooks (b:bs) = do
    putStrLn ("Book ID: " ++ show (bookId b))
    putStrLn ("Title: " ++ title b)
    putStrLn ("Author: " ++ author b)

    if available b
        then putStrLn "Status: Available"
        else putStrLn "Status: Not Available"

    putStrLn "-------------------------"
    displayBooks bs


-- Add book
addBook :: Library -> Book -> Library
addBook lib book = book : lib


-- Borrow book
borrowBook :: Int -> Library -> Library
borrowBook bid = map borrow
  where
    borrow book
      | bookId book == bid && available book =
            book {available = False}
      | otherwise = book


-- Return book
returnBook :: Int -> Library -> Library
returnBook bid = map ret
  where
    ret book
      | bookId book == bid =
            book {available = True}
      | otherwise = book


-- Search book
searchBook :: String -> Library -> [Book]
searchBook keyword lib =
    filter match lib
  where
    lowerKeyword = map toLower keyword

    match book =
        lowerKeyword `isInfixOf` map toLower (title book) ||
        lowerKeyword `isInfixOf` map toLower (author book)