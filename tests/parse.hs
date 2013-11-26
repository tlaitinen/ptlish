import Language.Ptlish
import System.IO
import System.Environment

main :: IO ()
main = do
    args <- getArgs
    c <- readFile (head args)
    let ptl = stringToPtlish c
    print ptl
    print $ simplify ptl
    
