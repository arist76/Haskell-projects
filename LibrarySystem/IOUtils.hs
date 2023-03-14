module IOUtils
  ( clearScreen,
    putOptions,
    putUnderlined,
    promptGetLine,
  )
where

import Control.Monad (foldM_)

clearScreen :: IO ()
clearScreen = do
  putStr "\ESC[2J"
  putStr "\ESC[1;1H"

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