
import System.Environment
import Data.List
import Data.Maybe


import CounterMatch.Parse
import CounterMatch.Eval



main = do
   args <- getArgs
   progName <- getProgName
   if (length args) < 2
        then do
                putStrLn "Usage: 3cm <input> <source>"
                putStrLn "where <input> is an integer and <source> is a Plotkin's 3 counter match programm"
        else do
                let file = args !! 1
                input <- readFile file
                putStrLn (case (parse3cm file input) of
                        Left err -> "Parsing error: " ++ (show err)
                        Right program -> (show program) ++ "\n" ++ (show (eval program (State (read (args!!0)::Int) 0 0 1)))

                            )

