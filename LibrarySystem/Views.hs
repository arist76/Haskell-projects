module Views
  ( homeView,
    bookView,
    memberView,
  )
where

import Control.Monad (foldM, foldM_)
import DB qualified
import Data.ByteString qualified as Models
import Data.Map qualified as Map
import Data.Maybe qualified as Mb
import IOUtils qualified
import Models qualified
import System.Exit (exitSuccess)

homeView :: IO ()
homeView = do
  putUnderlined "Home"
  putOptions
    [ "list books",
      "register book",
      "book info",
      "register member",
      "member info",
      "loan book",
      "return book"
    ]
  inp <- getLine
  case inp of
    "3" -> Views.bookView
    "5" -> Views.memberView
    "exit" -> do
      putStrLn "Bye!!!"
      exitSuccess
    _ -> do
      putStrLn "Exahusted!!!"

bookView :: IO ()
bookView = do
  IOUtils.clearScreen
  book <- promptGetLine "what is the name of the book?"
  IOUtils.clearScreen
  putUnderlined "Book Details"
  bookModel <- DB.getBook book
  Models.putDetails bookModel
  putStrLn $ replicate 50 '_'
  putOptions
    [ "home",
      "delete book",
      "edit book"
    ]
  inp <- getLine
  case inp of
    "1" -> return ()
    "2" -> do
      DB.deleteBook bookModel
    "3" -> do
      atr <-
        promptGetLine $
          "what is the new author name? (press 'Enter' to continue with: "
            ++ Models.author bookModel
            ++ ")"
      ttl <-
        promptGetLine $
          "what is the new title? (press 'Enter' to continue with: "
            ++ Models.title bookModel
            ++ ")"
      prc <-
        promptGetLine $
          "what is the new price? (press 'Enter' to continue with: "
            ++ show (Models.price bookModel)
            ++ ")"
      istr <-
        promptGetLine $
          "what is the new instore amount? (press 'Enter' to continue with: "
            ++ show (Models.inStore bookModel)
            ++ ")"

      DB.editBook
        Models.Book
          { Models.author = atr,
            Models.title = ttl,
            Models.price = if null prc then -1 else read prc :: Double,
            Models.inStore = if null istr then -1 else read istr :: Int,
            Models.loanedOut = []
          }

memberView :: IO ()
memberView = do
  IOUtils.clearScreen
  member <- promptGetLine "what is the name of the member"
  IOUtils.clearScreen
  putUnderlined "Member Details"
  memberModel <- DB.getMember member
  Models.putDetails memberModel
  putStrLn $ replicate 50 '_'
  putOptions
    [ "home",
      "delete member",
      "edit member"
    ]
  inp <- getLine
  case inp of
    "1" -> return ()
    "2" -> do
      DB.deleteMember memberModel
    "3" -> do
      mid <-
        promptGetLine $
          "what is the new member id number? (press 'Enter' to continue with: "
            ++ show (Models.member_id memberModel)
            ++ ")"
      fnm <-
        promptGetLine $
          "what is the new first name? (press 'Enter' to continue with: "
            ++ show (Models.member_id memberModel)
            ++ ")"
      lnm <-
        promptGetLine $
          "what is the new last name? (press 'Enter' to continue with: "
            ++ show (Models.member_id memberModel)
            ++ ")"

      -- DB.editMember
      print
        Models.Member
          { Models.member_id = if null mid then -1 else read mid :: Integer,
            Models.firstName = fnm,
            Models.lastName = lnm,
            Models.registeredDate = "",
            Models.booksBorrowed = []
          }
      getLine
      return ()

putOptions :: [String] -> IO ()
putOptions options = do
  foldM_ putOption 1 options
  where
    putOption :: Int -> String -> IO Int
    putOption acc x = do
      putStrLn (show acc ++ ") " ++ x)
      return (acc + 1)

putUnderlined :: String -> IO ()
putUnderlined str = do
  putStrLn str
  putStrLn (replicate (length str) '-')

promptGetLine :: String -> IO String
promptGetLine prompt = do
  putStrLn prompt
  getLine

{-
name, options + prompts +  ,
-}
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

-- mp = Map.fromList [("1#author", ["JK rowlings"]), ("2#title", ["Harry potter"]), ("3#price", ["1200"]), ("4#Loaned out to", ["surafel fikru", "abebe melese, genet jemal"])]