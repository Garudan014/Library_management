module FileHandler where

import Library
import System.Directory

saveLibrary :: Library -> IO ()
saveLibrary lib =
    writeFile "library.txt" (show lib)


loadLibrary :: IO Library
loadLibrary = do
    exists <- doesFileExist "library.txt"

    if exists
        then do
            contents <- readFile "library.txt"
            return (read contents)
        else
            return []