{-# LANGUAGE FlexibleContexts #-}
import Parselib
import System.IO
import System.Environment
import Control.Monad
import Control.Monad.State
import Control.Monad.Reader hiding (ask)
import Control.Monad.Trans
import Control.Monad.Trans.State hiding (get, put)
import Control.Monad.Trans.Reader
import Control.Monad.IO.Class --liftIO
import Data.Char
import Data.Array.IO

data Sexpr = Symbol String | Character Char | Number Int | Boolean Bool | Nil | Void | Cons {car :: Sexpr, cdr :: Sexpr} | Input (Char, String) | Let (Char, Int) | Unary {name :: String, func1 :: Sexpr -> Sexpr} | Binary {name :: String, func2 :: Sexpr -> Sexpr -> Sexpr}
--data Basic = INPUT (Char, String) | LET Sexpr Sexpr
--type KeyValue = (Char, Sexpr)

instance Eq Sexpr where
  Symbol x == Symbol y = x == y
  Number x == Number y = x == y
  Boolean x == Boolean y = x == y
  Nil == Nil = True

instance Show Sexpr where
  show (Symbol x) = x
  show (Number x) = show x
  show (Boolean x) = show x
  show (Character c) = show c
  show Nil = "[]"

--type Parser a = ReaderT Environment (StateT String IO) a

misc :: Char -> Bool
misc x = elem x "<>~!$^&+-*/=_?@"

legalFirst :: Char -> Bool
legalFirst x = misc x || isAlpha x

symbolic :: Char -> Bool
symbolic x = legalFirst x || isDigit x

symbol :: Parser Sexpr
symbol = do {c <- sat legalFirst; s <- many (sat symbolic); return (Symbol (c:s))}

number :: Parser Sexpr
number = do {n <- integer; return (Number n)}

variable :: Parser Int
variable = do
  (table,_) <- ask
  c <- sat isAlpha
  d <- liftIO $ readArray table c
  return (read d::Int)
  

--parse' :: String -> Environment -> Sexpr
--parse' line = fst . head . (apply b) $ line
--parse' line env = fst $ runStateT ((runReaderT b) env) line

--linenum :: Parser Sexpr
linenum = do {char '('; token $ many (sat isDigit); return Void}

--expr :: Parser Int
expr = term `chainl1` addop
term = factor `chainl1` mulop
factor = digit +++ variable +++ do {symb "("; e <- expr; symb ")"; return e}

addop = do {symb "+"; return (+)} +++ do {symb "-"; return (-)}
mulop = do {symb "*"; return (*)} +++ do {symb "/"; return (div)}

--b :: Parser Sexpr
b = input +++ var +++ let' +++ return Void

--input :: Parser Sexpr
input = do
  char '('
  token $ many (sat isDigit)
  token $ string "input"
  char '"'
  q <- many (sat (/= '"'))
  token $ char '"'
  c <- token $ alphanum
  char ')'
  return (Input (c, q))

--let' :: Parser Sexpr
let' = do
  char '('
  token $ many (sat isDigit)
  token $ string "let"
  c <- token $ sat isAlpha
  token $ char '='
  s <- many item
  let s' = take ((length s) - 1) s
  put s
  e <- expr
  return (Let (c, e))
  
  
--var :: Parser Sexpr
var = do
  char '('
  token $ many (sat isDigit)
  c <- token $ (sat isAlpha)
  char ')'  
  return (Character c)

--Eval-------------

--eval :: Sexpr -> ReaderT Environment IO Sexpr
eval sexpr@(Input (c, s)) = do
  (table, _) <- ask
  liftIO $ writeArray table c s
  return sexpr

eval sexpr@(Symbol s) = return sexpr 

eval Void = return Void
--  return (Symbol "Failed")

eval sexpr@(Let (c, s)) = do
  liftIO $ putStrLn $ "Let sexpr: " ++ (show s)
  
  return Void

--type Environment = (IOArray Char String, IOArray Int String)

basic :: Int -> ReaderT Environment IO ()
basic n = do
  liftIO . putStr $ (show n) ++ "> "
  env@(sTable, prog) <- ask
  line <- liftIO $ getLine
  if line == "quit" then return () else do
    liftIO $ writeArray prog n line
    --result <- eval (parse' line env)
    result <- eval (fst $ (runStateT (runReaderT b) env) line)
    case result of
      Void -> do
        --liftIO $ putStrLn "Failed parse"
        basic (n + 1)
      (Input (c, s)) -> do
        liftIO $ putStrLn "Input result"
        liftIO $ putStrLn s
        sym <- liftIO $ getLine
        liftIO $ writeArray sTable c sym
        basic (n + 1)
      x -> do
        --x <- eval result'
        liftIO $ putStrLn (show x)
        basic (n + 1)         

main :: IO ()
main = do
  --[fName] <- getArgs
  --handle <- openFile fName ReadMode
  hSetBuffering stdout NoBuffering
  arr1 <- newArray ('a', 'z') "" :: IO (IOArray Char String)
  arr2 <- newArray (1,200) "" :: IO (IOArray Int String)
  let environment = (arr1, arr2)
  runReaderT (basic 1) environment
