module Library where

import Book
import Data.List (isInfixOf, sortOn)
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

    putStrLn "----------------------"
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


-- Search by title or author
searchBook :: String -> Library -> [Book]
searchBook keyword lib =
    filter match lib
  where
    lowerKeyword = map toLower keyword

    match book =
        lowerKeyword `isInfixOf` map toLower (title book) ||
        lowerKeyword `isInfixOf` map toLower (author book)


-- Search by Book ID
searchById :: Int -> Library -> [Book]
searchById bid lib =
    filter (\b -> bookId b == bid) lib


-- Show available books
availableBooks :: Library -> [Book]
availableBooks =
    filter available


-- Show borrowed books
borrowedBooks :: Library -> [Book]
borrowedBooks =
    filter (\b -> not (available b))


-- Library statistics
libraryStats :: Library -> IO ()
libraryStats lib = do
    let total = length lib
    let availableCount = length (filter available lib)
    let borrowedCount = total - availableCount

    putStrLn ("Total Books: " ++ show total)
    putStrLn ("Available Books: " ++ show availableCount)
    putStrLn ("Borrowed Books: " ++ show borrowedCount)


-- Sort books
sortByTitle :: Library -> Library
sortByTitle =
    sortOn title


sortByAuthor :: Library -> Library
sortByAuthor =
    sortOn author