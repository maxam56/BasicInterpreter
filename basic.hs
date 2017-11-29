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

data Sexpr = Symbol String | Character Char | Number Int | Boolean Bool | Nil | Void | Cons {car :: Sexpr, cdr :: Sexpr} | Input (Char, String)
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

parse' :: String -> Sexpr
parse' line = fst . head . (apply b) $ line

eval :: Sexpr -> ReaderT Environment IO Sexpr
eval (Input (c, s)) = do
  (table, _) <- ask
  liftIO $ writeArray table c s
  liftIO $ putStrLn $ "Added " ++ s ++ " to table"
  return Void

eval (Character c) = do
  (table, _) <- ask
  q <- liftIO $ readArray table c 
  liftIO $ putStrLn q
  s <- liftIO $ getLine
  return (Symbol s)

eval Void = do
  return (Symbol "Failed")

b :: Parser Sexpr
b = input +++ var +++ (return Void)

input :: Parser Sexpr
input = do
  char '('
  token $ many (sat isDigit)
  token $ string "input"
  char '"'
  q <- many alphanum
  token $ char '"'
  c <- token $ alphanum
  char ')'
  return (Input (c, q))

var :: Parser Sexpr
var = do
  char '('
  token $ many (sat isDigit)
  c <- token $ (sat isAlpha)
  char ')'  
  return (Character c)

--Should hold the symbol table and program <- use IOArray at some point
type Environment = (IOArray Char String, IOArray Int String)

basic :: Int -> ReaderT Environment IO ()
basic n = do
  liftIO . putStr $ "> "
  --let s = (Number 5)
  (sTable, prog) <- ask
  --a <- liftIO $ readArray arr1 'a'
  --liftIO $ writeArray arr1 'a' (Number 5)
  --b <- liftIO $ readArray arr1 'a'
  --liftIO $ putStrLn $ "Was" ++ (show a) ++ " now " ++ (show b)
  --m <- readArray(arr1 'a')
  --print m
  --get line and put into IOArray
  line <- liftIO $ getLine
  if line == "quit" then return () else do
    liftIO $ writeArray prog n line
    result <- eval (parse' line)
    case result of
      Void -> basic (n + 1)
      result' -> do
        x <- eval result'
        liftIO $ putStrLn (show x)
        basic (n + 1)         
  --liftIO $ writeArray prog n line
  --liftIO $ writeArray sTable c s
  --e <- liftIO $ readArray sTable c
  
   



--main :: IO ()
main = do
  --[fName] <- getArgs
  --handle <- openFile fName ReadMode
  arr1 <- newArray ('a', 'z') "" :: IO (IOArray Char String)
  arr2 <- newArray (1,200) "" :: IO (IOArray Int String)
  let environment = (arr1, arr2)
  runReaderT (basic 1) environment
