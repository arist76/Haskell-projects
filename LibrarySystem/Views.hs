module Views (homeView) where

import Control.Monad (foldM, foldM_)
import Data.Map qualified as Map
import Data.Maybe qualified as Mb
import GHC.Base (IO (IO))

homeView :: IO ()
homeView = do
  putOptions
    [ "list books",
      "register book",
      "book info",
      "register member",
      "member info",
      "loan book",
      "return book"
    ]

bookView :: IO ()
bookView = do
  putUnderlined "Book Details"
  putStrLn $ replicate 50 '_'
  putOptions
    [ "delete book",
      "edit book"
    ]

putOptions :: [String] -> IO ()
putOptions options = do
  foldM_ putOption 1 options
  where
    putOption :: Int -> String -> IO Int
    putOption acc x = do
      putStrLn (show acc ++ ") " ++ x)
      return (acc + 1)

putDetails :: Map.Map String [String] -> IO (Map.Map String [String])
putDetails details = do
  foldM putDetail Map.empty (Map.keys details)
  where
    putDetail :: Map.Map String [String] -> String -> IO (Map.Map String [String])
    putDetail acc x
      | length x == 1 = do
          putStrLn (x ++ " : " ++ head val)
          return (Map.delete x details)
      | otherwise = do
          foldM_ putListDetail () (tail val)
          return (Map.delete x details)
      where
        val = Mb.fromMaybe (error "key not found") (Map.lookup x details)
        putListDetail :: () -> String -> IO ()
        putListDetail acc2 x2 = do
          putStrLn (replicate (length x + 3) ' ' ++ x2)

putUnderlined :: String -> IO ()
putUnderlined str = do
  putStrLn str
  putStrLn (replicate (length str) '-')

{-
MAIN
----
list books
register book
book info
register member
member info
loan book
return book

BOOK INFO
---------
@ book details
delete book
edit book

MEMBER INFO
-----------
@ member details
delete member
edit member
-}
