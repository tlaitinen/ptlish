import Language.Ptlish
import System.IO
import System.Environment
main :: IO ()
main = do
    args <- getArgs
    c <- readFile (head args)
    print $ stringToPtlish c
    
