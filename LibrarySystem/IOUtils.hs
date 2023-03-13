module IOUtils (clearScreen) where

clearScreen :: IO ()
clearScreen = do
  putStr "\ESC[2J"
  putStr "\ESC[1;1H"
