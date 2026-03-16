module FileHandler where

import Library
import System.Directory

-- Save library to file
saveLibrary :: Library -> IO ()
saveLibrary lib =
    writeFile "library.txt" (show lib)


-- Load library from file
loadLibrary :: IO Library
loadLibrary = do
    exists <- doesFileExist "library.txt"

    if exists
        then do
            contents <- readFile "library.txt"
            return (read contents)
        else
            return []