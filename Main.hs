module Main where

import Book
import Library
import FileHandler

main :: IO ()
main = do
    lib <- loadLibrary
    menu lib


menu :: Library -> IO ()
menu lib = do
    putStrLn "\n===== Library Management System ====="
    putStrLn "1. Display Books"
    putStrLn "2. Add Book"
    putStrLn "3. Borrow Book"
    putStrLn "4. Return Book"
    putStrLn "5. Search by Title/Author"
    putStrLn "6. Search by Book ID"
    putStrLn "7. Show Available Books"
    putStrLn "8. Show Borrowed Books"
    putStrLn "9. Library Statistics"
    putStrLn "10. Sort Books by Title"
    putStrLn "11. Sort Books by Author"
    putStrLn "12. Save and Exit"
    putStrLn "Enter your choice:"

    choice <- getLine

    case choice of

        "1" -> do
            displayBooks lib
            menu lib

        "2" -> do
            putStrLn "Enter Book ID:"
            bid <- readLn

            putStrLn "Enter Title:"
            t <- getLine

            putStrLn "Enter Author:"
            a <- getLine

            let newBook = Book bid t a True
            let newLib = addBook lib newBook

            putStrLn "Book added!"
            menu newLib

        "3" -> do
            putStrLn "Enter Book ID to borrow:"
            bid <- readLn

            if any (\b -> bookId b == bid) lib
                then do
                    let newLib = borrowBook bid lib
                    putStrLn "Book borrowed!"
                    menu newLib
                else do
                    putStrLn "Book ID not found."
                    menu lib

        "4" -> do
            putStrLn "Enter Book ID to return:"
            bid <- readLn

            if any (\b -> bookId b == bid) lib
                then do
                    let newLib = returnBook bid lib
                    putStrLn "Book returned!"
                    menu newLib
                else do
                    putStrLn "Book ID not found."
                    menu lib

        "5" -> do
            putStrLn "Enter title or author:"
            keyword <- getLine

            let result = searchBook keyword lib

            if null result
                then putStrLn "Book not found."
                else displayBooks result

            menu lib

        "6" -> do
            putStrLn "Enter Book ID:"
            bid <- readLn

            let result = searchById bid lib

            if null result
                then putStrLn "Book not found."
                else displayBooks result

            menu lib

        "7" -> do
            displayBooks (availableBooks lib)
            menu lib

        "8" -> do
            displayBooks (borrowedBooks lib)
            menu lib

        "9" -> do
            libraryStats lib
            menu lib

        "10" -> do
            displayBooks (sortByTitle lib)
            menu lib

        "11" -> do
            displayBooks (sortByAuthor lib)
            menu lib

        "12" -> do
            saveLibrary lib
            putStrLn "Library saved."

        _ -> do
            putStrLn "Invalid choice!"
            menu lib