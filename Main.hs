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
    putStrLn "5. Search Book"
    putStrLn "6. Save and Exit"
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

            putStrLn "Book added successfully!"
            menu newLib

        "3" -> do
            putStrLn "Enter Book ID to borrow:"
            bid <- readLn

            let newLib = borrowBook bid lib
            putStrLn "Book borrowed!"
            menu newLib

        "4" -> do
            putStrLn "Enter Book ID to return:"
            bid <- readLn

            let newLib = returnBook bid lib
            putStrLn "Book returned!"
            menu newLib

        "5" -> do
            putStrLn "Enter title or author:"
            keyword <- getLine

            let result = searchBook keyword lib
            displayBooks result
            menu lib

        "6" -> do
            saveLibrary lib
            putStrLn "Library saved successfully!"

        _ -> do
            putStrLn "Invalid choice!"
            menu lib