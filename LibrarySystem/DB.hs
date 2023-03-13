module DB
  ( getBook,
    deleteBook,
    editBook,
    getMember,
    deleteMember,
    editMember,
  )
where

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

getMember :: String -> IO Models.Member
getMember member = do
  return Models.Member {Models.member_id = 1, Models.firstName = "surafel", Models.lastName = "fikru", Models.registeredDate = "12/12/2000", Models.booksBorrowed = []}

deleteMember :: Models.Member -> IO ()
deleteMember member = do
  return ()

editMember :: Models.Member -> IO ()
editMember member = do
  return ()