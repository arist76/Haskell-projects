{-# LANGUAGE OverloadedStrings #-}

module Views (homeView, bookView, memberView) where

import Control.Monad (foldM, foldM_)
import qualified DB
import qualified Data.Map as Map
import qualified Data.Maybe as Mb
import qualified Data.Text as T
import Data.Time.Clock (getCurrentTime)
import qualified IOUtils
import qualified Models
import System.Exit (exitSuccess)

homeView :: IO ()
homeView = do
  IOUtils.putUnderlined "Home"
  IOUtils.putOptions
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
    "1" -> bookListView
    "2" -> registerBookView
    "3" -> bookView
    "4" -> registerMemberView
    "5" -> memberView
    "6" -> loanBookView
    "7" -> returnBookView
    "exit" -> do
      putStrLn "Bye!!!"
      exitSuccess
    _ -> do
      putStrLn "Exahusted!!!"

bookView :: IO ()
bookView = do
  IOUtils.clearScreen
  book <- IOUtils.promptGetLine "what is the name of the book?"
  IOUtils.clearScreen
  IOUtils.putUnderlined "Book Details"
  bookModel <- DB.getBook book
  Models.putDetails bookModel
  putStrLn $ replicate 50 '_'
  IOUtils.putOptions
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
        IOUtils.promptGetLine $
          "what is the new author name? (press 'Enter' to continue with: "
            ++ Models.author bookModel
            ++ ")"
      ttl <-
        IOUtils.promptGetLine $
          "what is the new title? (press 'Enter' to continue with: "
            ++ Models.title bookModel
            ++ ")"
      prc <-
        IOUtils.promptGetLine $
          "what is the new price? (press 'Enter' to continue with: "
            ++ show (Models.price bookModel)
            ++ ")"
      istr <-
        IOUtils.promptGetLine $
          "what is the new instore amount? (press 'Enter' to continue with: "
            ++ show (Models.inStore bookModel)
            ++ ")"

      DB.editBook
        Models.Book
          { Models.bookID = Models.bookID bookModel,
            Models.author =
              if null atr
                then Models.author bookModel
                else atr,
            Models.title =
              if null ttl
                then Models.title bookModel
                else ttl,
            Models.price =
              if null prc
                then Models.price bookModel
                else read prc :: Double,
            Models.inStore =
              if null istr
                then Models.inStore bookModel
                else read istr :: Int
          }

memberView :: IO ()
memberView = do
  IOUtils.clearScreen
  member <- IOUtils.promptGetLine "what is the id of the member"
  IOUtils.clearScreen
  IOUtils.putUnderlined "Member Details"
  memberModel <- DB.getMember (read member :: Integer)
  Models.putDetails memberModel
  putStrLn $ replicate 50 '_'
  IOUtils.putOptions
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
      fnm <-
        IOUtils.promptGetLine $
          "what is the new first name? (press 'Enter' to continue with: "
            ++ show (Models.firstName memberModel)
            ++ ")"
      lnm <-
        IOUtils.promptGetLine $
          "what is the new last name? (press 'Enter' to continue with: "
            ++ show (Models.lastName memberModel)
            ++ ")"

      DB.editMember
        Models.Member
          { Models.member_id = Models.member_id memberModel,
            Models.firstName =
              if null fnm
                then Models.firstName memberModel
                else fnm,
            Models.lastName =
              if null lnm
                then Models.lastName memberModel
                else lnm,
            Models.registeredDate = undefined
          }
      getLine
      return ()

bookListView :: IO ()
bookListView = do
  books <- DB.getAllBooks
  putBooks books
  where
    putBooks books = do
      IOUtils.clearScreen
      foldM_ putBook () books
      IOUtils.putOptions ["home"]
      inp <- getLine
      if inp == "1"
        then return ()
        else do
          putBooks books
    putBook :: () -> Models.Book -> IO ()
    putBook acc x = do
      putStrLn $ Models.title x ++ " by " ++ Models.author x

registerBookView :: IO ()
registerBookView = do
  atr <- IOUtils.promptGetLine "Enter the authors name"
  ttl <- IOUtils.promptGetLine "Enter the title of the book"
  prc <- IOUtils.promptGetLine "Enter the price of the book"
  istr <- IOUtils.promptGetLine "Enter quantity of the books"

  DB.registerBook
    Models.Book
      { Models.bookID = undefined,
        Models.author =
          if null atr
            then error "Book.author cannot be null"
            else atr,
        Models.title =
          if null ttl
            then error "Book.title cannot be null"
            else ttl,
        Models.price =
          if null prc
            then error "Book.price cannot be null"
            else read prc :: Double,
        Models.inStore =
          if null istr
            then error "Book.inStore cannot be null"
            else read istr :: Int
      }

registerMemberView :: IO ()
registerMemberView = do
  IOUtils.clearScreen
  mid <- IOUtils.promptGetLine "Enter the id number of the member"
  fnm <- IOUtils.promptGetLine "Enter the first name of the member"
  lnm <- IOUtils.promptGetLine "Enter the second name of the member"
  rdt <- getCurrentTime

  DB.registerMember
    Models.Member
      { Models.member_id =
          if null mid
            then error "Book.member_id cannot be null"
            else read mid :: Integer,
        Models.firstName =
          if null fnm
            then error "Book.firstName cannot be null"
            else fnm,
        Models.lastName =
          if null lnm
            then error "Book.lastName cannot be null"
            else lnm,
        Models.registeredDate = show rdt
      }

loanBookView :: IO ()
loanBookView = do
  IOUtils.clearScreen
  mid <- IOUtils.promptGetLine "What is the ID of the member borrowing the book"
  ttl <- IOUtils.promptGetLine "What is the title of the book being loaned out"
  ldt <- getCurrentTime

  bk <- DB.getBook ttl

  DB.createLoan
    Models.Loan
      { Models.loanID = undefined,
        Models.book = Models.bookID bk,
        Models.member = read mid :: Integer,
        Models.loan_date = show ldt,
        Models.returned = False
      }

returnBookView :: IO ()
returnBookView = do
  IOUtils.clearScreen
  mid <- IOUtils.promptGetLine "What is the ID of the member returning the book"
  ttl <- IOUtils.promptGetLine "what is the title of the book being returned"

  bk <- DB.getBook ttl

  ln <- DB.getLoan (read mid :: Integer, Models.bookID bk)
  DB.editLoan
    Models.Loan
      { Models.loanID = Models.loanID ln,
        Models.book = Models.book ln,
        Models.member = Models.member ln,
        Models.loan_date = Models.loan_date ln,
        Models.returned = True
      }