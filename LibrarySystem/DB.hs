module DB
  ( getBook,
    deleteBook,
    editBook,
    getMember,
    deleteMember,
    editMember,
    getAllBooks,
    registerBook,
    registerMember,
    createLoan,
    getLoan,
    editLoan,
  )
where

import Models (Loan (loan_date))
import Models qualified

getBook :: String -> IO Models.Book
getBook title = do
  return Models.Book {Models.author = "JK rowlings", Models.title = "Harry potter", Models.price = 12.99, Models.inStore = 10, Models.loanedOut = ["surafel fikru", "abebe melese", "abel dopper"]}

deleteBook :: Models.Book -> IO ()
deleteBook book = do
  return ()

editBook :: Models.Book -> IO ()
editBook book = do
  return ()

getAllBooks :: IO [Models.Book]
getAllBooks = do
  bk <- getBook ""
  return [bk, bk, bk]

registerBook :: Models.Book -> IO ()
registerBook new_book = do
  return ()

getMember :: String -> IO Models.Member
getMember member = do
  return Models.Member {Models.member_id = 1, Models.firstName = "surafel", Models.lastName = "fikru", Models.registeredDate = "12/12/2000", Models.booksBorrowed = []}

deleteMember :: Models.Member -> IO ()
deleteMember member = do
  return ()

editMember :: Models.Member -> IO ()
editMember member = do
  return ()

registerMember :: Models.Member -> IO ()
registerMember member = do
  return ()

createLoan :: Models.Loan -> IO ()
createLoan loan = do
  return ()

editLoan :: (String, String) -> Models.Loan -> IO ()
editLoan from to = do
  return ()

getLoan :: (String, String) -> IO Models.Loan
getLoan loan = do
  return undefined