{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <&>" #-}
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad

symbol :: Parser Char
symbol = oneOf "!$%&|*+ -/: <=? >@^_~#"

spaces :: Parser ()
spaces = skipMany1 space

escapedChars :: Parser String
escapedChars = do
            char '\\'
            x <- oneOf "\\\""
            return [x]

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
    Left err -> "No match: " ++ show err
    Right val -> "Found value"

-- defining Lisp data types
data LispVal = Atom String
            | List [LispVal]
            | DottedList [LispVal] LispVal
            | Number Integer
            | String String
            | Bool Bool

parseString :: Parser LispVal
parseString = do
            char '"'
            x <- many (noneOf "\"") -- oneOf ([Char])
            char '"'
            return $ String x

parseAtom :: Parser LispVal
parseAtom = do
            first <- letter <|> symbol
            rest <- many (letter <|> digit <|> symbol)
            let atom = [first] ++ rest
            return $ case atom of
                "#t" -> Bool True
                "#f" -> Bool False
                otherwise -> Atom atom

parseNumber :: Parser LispVal
parseNumber = do
        many1 digit >>= return . Number . read
-- (return . Number . read) digits
-- liftM - promoting function into a monad
-- liftM (Number . read) $ many1 digit        


parseExpr :: Parser LispVal
parseExpr = parseAtom <|> parseString
-- inserting this into our readExpr function

main :: IO ()
main = do
    args <- getArgs
    putStrLn (readExpr (head args))
