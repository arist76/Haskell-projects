import Views qualified

main = do
  home

home :: IO ()
home = do
  welcome
  homeLoop

homeLoop = do
  Views.homeView
  inp <- getLine
  if inp == "exit"
    then putStrLn "Bye!!!"
    else homeLoop

welcome :: IO ()
welcome = do
  putStrLn "Welcome to the library system"
