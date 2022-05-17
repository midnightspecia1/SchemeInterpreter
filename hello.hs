module Main where 
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)

main :: IO ()
main = do 
    putStrLn "Enter the message!"
    line <- getLine
    putStrLn line