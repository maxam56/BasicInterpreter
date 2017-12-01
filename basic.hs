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

data Sexpr = Symbol String | Variable Char | Number Float | Boolean Bool | Input (Char, String) | Let (Char, String) | Prints [Sexpr] | Void | Failed | End

instance Eq Sexpr where
  Symbol x == Symbol y = x == y
  Number x == Number y = x == y
  Boolean x == Boolean y = x == y

instance Show Sexpr where
  show (Symbol x) = x
  show (Number x) = show x
  show (Boolean x) = show x
  show Failed = "Failed"
  --show (Variable c) = 
  --show (Let (c, i)) = (show c) ++ "=" ++ i

--Parse----------------------------------------

misc :: Char -> Bool
misc x = elem x "<>~!$^&+-*/=_?@"

legalFirst :: Char -> Bool
legalFirst x = misc x || isAlpha x

symbolic :: Char -> Bool
symbolic x = legalFirst x || isDigit x

symbol :: Parser Sexpr
symbol = do {c <- sat legalFirst; s <- many (sat symbolic); return (Symbol (c:s))}

--number :: Parser Sexpr
--number = do {n <- integer; return (Number n)}

--variable :: Num a => Parser a
--variable = fractional +++ integer
--fractional :: Parser Float

variable :: Parser Float
variable = do
  (table,_) <- ask
  c <- token $ sat isAlpha
  d <- liftIO $ readArray table c
  return (read d::Float)

floatOrInt :: Parser Float
floatOrInt = float +++ intToFloat

float :: Parser Float
float = do {fst <- many (sat isDigit); d <- char '.'; lst <- token $ many (sat isDigit) ; return (read (fst ++ [d] ++ lst) :: Float)}

intToFloat :: Parser Float
intToFloat = do {d <- int; return (fromIntegral d)}

parse' :: String -> Environment -> IO Sexpr
parse' line env = do
  (sexpr, _) <- runStateT ((runReaderT b) env) line
  return sexpr
 
linenum = do {char '('; token $ many (sat isDigit); return Void}

expr :: Parser Float
expr = term `chainl1` addop
term = factor `chainl1` mulop
factor = floatOrInt +++ variable +++ do {symb "("; e <- expr; symb ")"; return e}

addop = do {symb "+"; return (+)} +++ do {symb "-"; return (-)}
mulop = do {symb "*"; return (*)} +++ do {symb "/"; return (/)}

b :: Parser Sexpr
b = input +++ let' +++ print' +++ end +++ return Failed

input :: Parser Sexpr
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

let' :: Parser Sexpr
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
  return (Let (c, (show e)))

p :: Parser Sexpr
p = s +++ e

par = do {char '\n'; return (Symbol "\n")}

e = do {e <- expr; return (Number e)}

s :: Parser Sexpr
s = do
  char '"'
  str <- many $ sat (/= '"')
  token $ char '"'
  --liftIO $ putStrLn $ "Got symbol " ++ str
  return (Symbol str)

print' :: Parser Sexpr
print' = do
  linenum
  symb "print"
  sexprs <- many p
  --char ')'
  return (Prints sexprs)
  
end = do {linenum; symb "end"; return End}
--Eval--------------------------------------

eval :: Sexpr -> ReaderT Environment IO Sexpr
eval sexpr@(Input (c, q)) = do
  (table, _) <- ask
  liftIO $ putStrLn q
  v <- liftIO $ getLine
  liftIO $ writeArray table c v
  --liftIO $ putStrLn v
  return Void

eval sexpr@(Symbol s) = return sexpr 

eval Void = return Void
--  return (Symbol "Failed")

eval sexpr@(Let (c, s)) = do
  (table,_) <- ask
  liftIO $ writeArray table c s
  liftIO $ putStrLn ""
  return Void
  --liftIO $ putStrLn $ show sexpr
  --return Void

eval sexpr@(Variable c) = do
  (table, _) <- ask
  v <- liftIO $ readArray table c
  liftIO $ putStrLn $ v
  return Void


eval (Prints []) = do {liftIO $ putStr $ "\n"; return Void}
eval sexpr@(Prints (s:ss)) = do
  liftIO $ putStr $ (show s)
  eval (Prints ss)
  --return Void

eval End = return End

eval Failed = return Failed

checkEof :: Handle -> IO Bool
checkEof h = do
  if h == stdin then return False else hIsEOF h
  --eof <- hIsEOF h
  --if eof then return True else return False

basic :: Int -> Handle -> ReaderT Environment IO ()
basic n h = do
  --if h /= stdin then do {eof <- liftIO $ hIsEOF h} else do {let eof = false}
  eof <- liftIO $ checkEof h
  if eof then return () else do
    env@(sTable, prog) <- ask
    liftIO . putStr $ "> "
    line <- liftIO $ hGetLine h
    if line == "quit" then return () else do
      liftIO $ writeArray prog n line
      s <- liftIO $ parse' line env
      --liftIO $ putStrLn (show s)
      e <- eval s
      case e of
        Failed -> do {liftIO . putStrLn $ (show e); basic n h}
        End -> do {liftIO . putStrLn $ ""; return ()}
        _ -> basic (n+1) h       

main :: IO ()
main = do
      
  --[fName] <- getArgs
  --handle <- openFile fName ReadMode
  hSetBuffering stdout NoBuffering
  arr1 <- newArray ('a', 'z') "" :: IO (IOArray Char String)
  arr2 <- newArray (1,200) "" :: IO (IOArray Int String)
  let env = (arr1, arr2)
  args <- getArgs
  case args of
    [fName] -> do
      handle <- openFile fName ReadMode
      hGetLine handle --Read the (define foo'( 
      runReaderT (basic 1 handle) env
    _ -> runReaderT (basic 1 stdin) env
