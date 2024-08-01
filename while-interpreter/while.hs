import System.Environment (getArgs)
import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.String

import Data.Map (Map, insert, findWithDefault, empty)
import Data.Bifunctor (second)

data Program = Zero String | Inc String String | Dec String String | While String String Program | Block [Program]
    deriving Show

type Vars = Map String Integer

parseVar :: Parser String
parseVar = do
    a <- letter
    as <- many $ choice [alphaNum, char '_']
    pure $ a : as

parseOp :: Char -> (String -> String -> Program) -> Parser Program
parseOp operator cons = do
    var1 <- parseVar
    spaces
    string ":="
    spaces
    var2 <- parseVar
    spaces
    char operator
    spaces
    char '1'
    pure $ cons var1 var2

parseZero :: Parser Program
parseZero = do
    var <- parseVar
    spaces
    string ":="
    spaces
    char '0'
    pure $ Zero var

parseInc :: Parser Program
parseInc = parseOp '+' Inc

parseDec :: Parser Program
parseDec = parseOp '-' Dec

parseAsgt :: Parser Program
parseAsgt = choice [try parseZero, try parseInc, try parseDec]

parseWhile :: Parser Program
parseWhile = do
    string "while"
    spaces
    var1 <- parseVar
    spaces
    string "!="
    spaces
    var2 <- parseVar
    spaces
    string "do"
    spaces
    stmt <- parseStmt
    pure $ While var1 var2 stmt

parseStmt :: Parser Program
parseStmt = choice [try parsePrg, try parseWhile, try parseAsgt]


parsePrg :: Parser Program
parsePrg = do
    seq <- between (string "begin" >> spaces) (spaces >> string "end") (sepBy parseStmt (char ';' >> spaces))
    pure $ Block seq

get :: String -> Vars -> Integer
get = findWithDefault 0

dec :: Integer -> Integer
dec x = max 0 (x - 1)

eval :: Vars -> Program -> Vars
eval vars (Zero var) = insert var 0 vars
eval vars (Inc var1 var2) = insert var1 (get var2 vars + 1) vars
eval vars (Dec var1 var2) = insert var1 (dec $ get var2 vars) vars
eval vars (While var1 var2 prog) = if (get var1 vars == get var2 vars) then vars else eval (eval vars prog) (While var1 var2 prog)
eval vars (Block []) = vars
eval vars (Block (x:xs)) = eval (eval vars x) (Block xs)

semantic :: Vars -> Program -> Integer
semantic vars prog = get "x1" (eval vars prog)

createVars :: Int -> [String] -> Vars
createVars _ [] = empty
createVars n (num : nums) = insert ("x" ++ show n) (read num) (createVars (n + 1) nums)

reader :: [String] -> IO ()
reader (filename : args) = do
    res <- parseFromFile parsePrg filename
    print $ second (semantic (createVars 1 args)) res

main :: IO ()
main = do
    args <- getArgs
    if null args
        then
            putStrLn "Expected filename with while-program."
        else
            reader args
