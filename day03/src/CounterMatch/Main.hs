
import System.Environment
import Data.List
import Data.Maybe

import CounterMatch.Parse
import CounterMatch.Eval




main = do
   args <- getArgs
   progName <- getProgName
   if (length args) < 1
        then do
                putStrLn "Usage: par3cm <source>"
                putStrLn "where <source> is a Plotkin's 3 counter match programm"
        else do
                let file = args !! 0
                input <- readFile file
                putStrLn (case (parse3cm file input) of
                        Left err -> "Parsing error: " ++ (show err)
                        Right program -> (show program) ++ "\n" ++ (showMap (eval program (bottom_map program)))

                            )

