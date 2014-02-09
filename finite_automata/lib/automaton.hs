import Regex
import AFN
import System.IO

mainLoop :: AFN -> [Char] -> IO()
mainLoop afn word = do end <- isEOF
                       if end then putStr ""
                       else do word <- getLine
                               if compute afn word then putStrLn "1"
                               else putStrLn "0"
                               mainLoop afn word

main = do regex <- getLine
          let afn = regexReader regex
          mainLoop afn ""

