{-# LANGUAGE DefaultSignatures #-}

module Models (Book (..), Member (..), Loan (..), toMap, putDetails) where

import Control.Monad (foldM, foldM_)
import qualified Data.Map as Map
import qualified Data.Maybe as Mb

data Loan = Loan
  { book :: Book,
    member :: Member,
    loan_date :: String,
    returned :: Bool
  }
  deriving (Show)

data Book = Book
  { author :: String,
    title :: String,
    price :: Double,
    inStore :: Int,
    loanedOut :: [String]
  }
  deriving (Show, Eq, Ord)

instance DetailedEntity Book where
  -- toMap :: Book -> Map.Map String [String]
  toMap b =
    Map.fromList
      [ ("1#author", [author b]),
        ("2#title", [title b]),
        ("3#price", [show $ price b]),
        ("4#in store", [show $ inStore b]),
        ("5#loaned out", loanedOut b)
      ]

data Member = Member
  { member_id :: Integer,
    firstName :: String,
    lastName :: String,
    registeredDate :: String,
    booksBorrowed :: [String]
  }
  deriving (Show, Eq, Ord)

instance DetailedEntity Member where
  -- toMap :: Member -> Map.Map String [String]
  toMap m =
    Map.fromList
      [ ("1#first name", [firstName m]),
        ("2#last name", [lastName m]),
        ("3#registeration date", [registeredDate m]),
        ("4#borrowed books", booksBorrowed m)
      ]

class DetailedEntity d where
  toMap :: d -> Map.Map String [String]
  putDetails :: d -> IO ()
  default putDetails :: d -> IO ()
  putDetails d = do
    foldM putDetail () (Map.keys details)
    where
      details = toMap d
      putDetail :: () -> String -> IO ()
      putDetail acc x
        | length val == 1 = do
            putStrLn (key ++ " : " ++ head val)
        | otherwise = do
            putStrLn (key ++ " : " ++ if null $ take 1 val then "nothing" else head val)
            foldM_ putListDetail () (drop 1 val)
        where
          val = Mb.fromMaybe (error "key not found") (Map.lookup x details)
          key = drop 1 (dropWhile ('#' /=) x)
          putListDetail :: () -> String -> IO ()
          putListDetail acc2 x2 = do
            putStrLn (replicate (length key + 3) ' ' ++ x2)
