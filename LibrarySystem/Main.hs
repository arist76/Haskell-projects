import IOUtils qualified
import Views qualified

main :: IO ()
main = do
  home

home :: IO ()
home = do
  welcome
  homeLoop

homeLoop :: IO ()
homeLoop = do
  Views.homeView
  IOUtils.clearScreen
  homeLoop

welcome :: IO ()
welcome = do
  putStrLn "Welcome to the library system"
