import System.Environment
Main :: IO ()

Main = getArgs >>= print . haqify . head

haqify s = "Haq! " ++ s
