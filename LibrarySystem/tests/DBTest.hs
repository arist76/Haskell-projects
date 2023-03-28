module DBTest where

import Control.Monad (foldM, foldM_)
import qualified DB
import qualified Models
import qualified System.Process as Bash
import qualified Test.HUnit as HUnit

setUpDb :: IO ()
setUpDb = Bash.callCommand "sqlite3 db/lib.db < db/insertTestData.sql"

flushDb :: IO ()
flushDb = Bash.callCommand "sqlite3 db/lib.db < db/flushDB.sql"

testGetBook :: HUnit.Test
testGetBook =
  HUnit.TestCase
    ( do
        doAssertion "JK rowlings" "Harry potter" 15.0 2
        doAssertion "George RR Martins" "Game of thrones" 22.0 10
        doAssertion "J.K. Rowling" "The Casual Vacancy" 18.0 5
        doAssertion "Stephenie Meyer" "Twilight" 12.0 8
        doAssertion "Dan Brown" "The Da Vinci Code" 20.0 3
        doAssertion "Agatha Christie" "Murder on the Orient Express" 15.0 6
        doAssertion "J.R.R. Tolkien" "The Lord of the Rings" 25.0 2
        doAssertion "Harper Lee" "To Kill a Mockingbird" 14.0 4
        doAssertion "Ernest Hemingway" "The Old Man and the Sea" 16.0 7
        doAssertion "Gabriel García Márquez" "One Hundred Years of Solitude" 19.0 9
    )
  where
    doAssertion :: String -> String -> Double -> Int -> HUnit.Assertion
    doAssertion author title price inStore =
      do
        book <- DB.getBook title
        HUnit.assertEqual (title ++ " author") (Models.author book) author
        HUnit.assertEqual (show price ++ " price") (Models.price book) price
        HUnit.assertEqual (show inStore ++ " inStore") (Models.inStore book) inStore

-- testGetBook =
--   HUnit.TestCase
--     ( do
--         foldM assertAll () doGetBook
--     )
--   where
--     books :: [String]
--     books = ["JK rowlings"]
--     getBookWithAcc :: [Models.Book] -> String -> IO [Models.Book]
--     getBookWithAcc acc x = do
--       book <- DB.getBook x
--       return (book : acc)
--     doGetBook :: [Models.Book]
--     doGetBook = foldM getBookWithAcc [] books
--     assertAuthor :: Models.Book -> String -> HUnit.Assertion
--     assertAuthor actual expected =
--       HUnit.assertEqual
--         ( Models.author actual
--             ++ " to "
--             ++ expected
--         )
--         (Models.author actual)
--         expected
--     assertPrice :: Models.Book -> Double -> HUnit.Assertion
--     assertPrice actual expected =
--       HUnit.assertEqual
--         ( show (Models.price actual)
--             ++ " to "
--             ++ show expected
--         )
--         (Models.price actual)
--         expected
--     assertInstore :: Models.Book -> Int -> HUnit.Assertion
--     assertInstore actual expected =
--       HUnit.assertEqual
--         ( show (Models.inStore actual)
--             ++ " to "
--             ++ show expected
--         )
--         (Models.inStore actual)
--         expected