{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}

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

import qualified Database.SQLite.Simple as Lite
import qualified Database.SQLite.Simple.FromRow as LiteFromRow
import qualified Models

-- | retrieves a book using a title which is a string
getBook :: String -> IO Models.Book
getBook title = do
  conn <- Lite.open "db/lib.db"
  resp <-
    head
      <$> ( Lite.query
              conn
              "SELECT * FROM Book WHERE (title = ?);"
              ( Lite.Only title
              ) ::
              IO [Models.Book]
          )
  Lite.close conn
  return resp

-- | deletes a book, it accepts 'Models.Book'
deleteBook :: Models.Book -> IO ()
deleteBook book = do
  conn <- Lite.open "db/lib.db"
  Lite.execute
    conn
    "DELETE FROM Book WHERE (id = ?);"
    (Lite.Only $ Models.bookID book)

-- | edits a books contents, it accepts a 'Models.Book' which is the updated book.
editBook :: Models.Book -> IO ()
editBook book = do
  conn <- Lite.open "db/lib.db"
  Lite.execute
    conn
    "UPDATE Book SET author=?, title=?, price=?, inStore=? WHERE id=?"
    (Models.author book, Models.title book, Models.price book, Models.inStore book, Models.bookID book)

-- | returns a list of all the contents of the Book table as a list of 'Models.Book'
getAllBooks :: IO [Models.Book]
getAllBooks = do
  conn <- Lite.open "db/lib.db"
  books <- (Lite.query_ conn "SELECT * FROM Book;" :: IO [Models.Book])
  Lite.close conn
  return books

-- | creates new books, accepts the 'Models.Book' which is to be created
registerBook :: Models.Book -> IO ()
registerBook new_book = do
  conn <- Lite.open "db/lib.db"
  Lite.execute
    conn
    "INSERT INTO Book (author, title, price, inStore) VALUES (? , ?, ?, ?);"
    (Models.author new_book, Models.title new_book, Models.price new_book, Models.inStore new_book)
  Lite.close conn

-- | retreives a member data using its id
getMember :: Integer -> IO Models.Member
getMember member = do
  conn <- Lite.open "db/lib.db"
  resp <- head <$> (Lite.query conn "SELECT * FROM Member WHERE (member_id = ?);" (Lite.Only member) :: IO [Models.Member])
  Lite.close conn
  return resp

-- | deletes a member, accepts a 'Models.Member' which is the member to delete
deleteMember :: Models.Member -> IO ()
deleteMember member = do
  conn <- Lite.open "db/lib.db"
  Lite.execute
    conn
    "DELETE FROM Member WHERE (member_id = ?);"
    (Lite.Only $ Models.member_id member)

-- | edits a member, accepts a 'Models.Member' which is the member to edit
editMember :: Models.Member -> IO ()
editMember member = do
  conn <- Lite.open "db/lib.db"
  Lite.execute
    conn
    "UPDATE Member SET firstName=?, lastName=? WHERE member_id=?"
    (Models.firstName member, Models.lastName member, Models.member_id member)

-- | creates a new member, accepts a 'Models.Member' which is the member to create
registerMember :: Models.Member -> IO ()
registerMember member = do
  conn <- Lite.open "db/lib.db"
  Lite.execute
    conn
    "INSERT INTO Member (member_id, firstName, lastName, registeredDate) VALUES (? , ?, ?, ?);"
    (Models.member_id member, Models.firstName member, Models.lastName member, Models.registeredDate member)
  Lite.close conn

-- | creates a new loan entity, accepts a 'Models.Loan' which is the loan to create
createLoan :: Models.Loan -> IO ()
createLoan loan = do
  conn <- Lite.open "db/lib.db"
  Lite.execute
    conn
    "INSERT INTO Loan (book, member, loanDate, returned) VALUES (?,?,?,?)"
    (Models.book loan, Models.member loan, Models.loan_date loan, Models.returned loan)

-- | edits loan data, accepts a 'Models.Loan' which is the loan to edit
editLoan :: Models.Loan -> IO ()
editLoan loan = do
  conn <- Lite.open "db/lib.db"
  Lite.execute
    conn
    "UPDATE Loan SET book=?, member=?, loanDate=?, returned=? WHERE loanID=?"
    (Models.book loan, Models.member loan, Models.loan_date loan, Models.returned loan, Models.loanID loan)

-- | retreives loan data, accepts an Integer pair representing the member id and book id
-- respectively
getLoan :: (Integer, Integer) -> IO Models.Loan
getLoan bookMember = do
  conn <- Lite.open "db/lib.db"
  resp <-
    head
      <$> ( Lite.query
              conn
              "SELECT * FROM Loan WHERE (member = ? AND book = ?);"
              bookMember ::
              IO [Models.Loan]
          )
  Lite.close conn
  return resp

-- | makes FromRow an interface of 'Models.Book'. creates an interface for the datase table
-- Book.
instance LiteFromRow.FromRow Models.Book where
  fromRow :: LiteFromRow.RowParser Models.Book
  fromRow =
    Models.Book
      <$> LiteFromRow.field
      <*> LiteFromRow.field
      <*> LiteFromRow.field
      <*> LiteFromRow.field
      <*> LiteFromRow.field

-- | makes FromRow an interface of 'Models.Member'. creates an interface for the datase table
-- Member.
instance LiteFromRow.FromRow Models.Member where
  fromRow :: LiteFromRow.RowParser Models.Member
  fromRow =
    Models.Member
      <$> LiteFromRow.field
      <*> LiteFromRow.field
      <*> LiteFromRow.field
      <*> LiteFromRow.field
