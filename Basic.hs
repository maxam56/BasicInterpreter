{-# LANGUAGE FlexibleContexts #-}

import Parselib
import Parselib(Sexpr(..))
import System.IO
import System.Environment
import Control.Monad
import Control.Monad.State
import Control.Monad.Reader hiding (ask)
import Control.Monad.Trans
import Control.Monad.Trans.State hiding (get, put)
import Control.Monad.Trans.Reader
import Control.Monad.IO.Class --liftIO
import Data.Char hiding (Control)
import Data.Array.IO
import Data.IORef


--Parse----------------------------------------

misc :: Char -> Bool
misc x = elem x "<>~!$^&+-*/=_?@"

legalFirst :: Char -> Bool
legalFirst x = misc x || isAlpha x

symbolic :: Char -> Bool
symbolic x = legalFirst x || isDigit x

symbol :: Parser Sexpr
symbol = do {c <- sat legalFirst; s <- many (sat symbolic); return (Symbol (c:s))}

varExpOrNum :: Sexpr -> Parser Float
varExpOrNum sexpr@(Number v) = return v
varExpOrNum (Symbol s) = do
  os <- get --save old state because we're going to clobber it...probably a better way
  put s
  (Number v) <- expr
  put os
  return v
  
varExpOrNum (For _ i _ _) = return (fromIntegral i)
varExpOrNum _ = mzero

variable :: Parser Float
variable = do
  (table,_,_,_) <- ask
  c <- token $ letter
  s <- liftIO $ readArray table c
  v <- varExpOrNum s
  return v

floatOrInt :: Parser Float
floatOrInt = float +++ intToFloat +++ mzero

float :: Parser Float
float = posFloat +++ negFloat
posFloat = do
  fst <- many (sat isDigit)
  d <- char '.'
  lst <- token $ many (sat isDigit)
  return (read (fst ++ [d] ++ lst) :: Float)
  
negFloat = do
  s <- char '-';
  fst <- many (sat isDigit)
  d <- char '.'
  lst <- token $ many (sat isDigit)
  return (read ((s:fst) ++ [d] ++ lst) :: Float)

--unary :: Parser Sexpr
unary = log' +++ int' +++ abs' +++ rnd' +++ sqrt'
int' = do
  symb "int"
  (Number n) <- expr
  let n' = fromIntegral (floor n)
  return n'

log' = do
  symb "log"
  (Number n) <- expr
  return (logBase(10) n)

abs' = do
  symb "abs"
--  symb "("
  (Number n) <- expr
--  symb ")"
  return $ abs(n)

rnd' = do
  symb "rnd"
--  symb "("
  (Number n) <- expr
--  symb ")"
  return $ fromIntegral(round(n))

sqrt' = do
  symb "sqrt"
--  symb "("
  (Number n) <- expr
--  symb ")"
  return $ sqrt(n)


--intToFloat :: Parser Sexpr
intToFloat = do {d <- int; return (fromIntegral d)}

linenum :: Parser Int
linenum = do {char '('; n <- token $ many (sat isDigit); return (read n)}

expr :: Parser Sexpr
expr = do {s <- (term `chainl1` addop); return (Number s)}
term = factor `chainl1` mulop
factor = unary +++ floatOrInt +++ variable +++ do {symb "("; (Number e) <- expr; symb ")"; return e}

addop = do {symb "+"; return (+)} +++ do {symb "-"; return (-)}
mulop = do {symb "*"; return (*)} +++ do {symb "/"; return (/)}



comparison :: Parser Bool
comparison = eq +++ gt +++ lt +++ gtEq +++ ltEq

value = floatOrInt +++ variable

eq = do
  token $ char '('
  v1 <- token $ value
  symb "="
  v2 <- token $ value
  token $ char ')'
  let b = ((==) v1 v2)
  return b
gt = do
  token $ char '('
  v1 <- token $ value
  symb ">"
  v2 <- token $ value
  token $ char ')'
  let b = ((>) v1 v2)
  return b
lt = do
  token $ char '('
  v1 <- token $ value
  symb "<"
  v2 <- token $ value
  token $ char ')'
  let b = ((<) v1 v2)
  return b
gtEq = do
  token $ char '('
  v1 <- token $ value
  symb ">="
  v2 <- token $ value
  token $ char ')'
  let b = ((>=) v1 v2)
  return b
ltEq = do
  token $ char '('
  v1 <- token $ value
  symb "<="
  v2 <- token $ value
  token $ char ')'
  let b = ((<=) v1 v2)
  return b

parse' :: String -> Environment -> IO Sexpr
parse' line env = do
  (sexpr, _) <- runStateT ((runReaderT basic') env) line
  return sexpr

basic' :: Parser Sexpr
basic' = input +++ let' +++ print' +++ end +++ if' +++ goSub +++ goto +++ for +++ next +++ return' +++ tab +++ return Failed

input :: Parser Sexpr
input = do
  linenum
  token $ symb "input"
  char '"'
  q <- many (sat (/= '"'))
  token $ char '"'
  c <- token $ letter
  char ')'  
  return (Input (c, q))

let' :: Parser Sexpr
let' = do
  linenum
  symb "let"
  c <- token $ sat isAlpha
  token $ char '='
  s <- many item
  let s' = take ((length s) - 1) s
  put s'
  e <- expr
  return (Let (c, e))

prnt :: Parser Sexpr
prnt = s +++ expr

s = str +++ tab

str :: Parser Sexpr
str = do
  char '"'
  str <- many $ sat (/= '"')
  token $ char '"'
  return (Symbol str)

print' :: Parser Sexpr
print' = do
  linenum
  symb "print"
  s <- (bang +++ noBang)
  return s

noBang = do
  sexprs <- many prnt
  return (Prints sexprs)

bang :: Parser Sexpr
bang = do
  symb "!"
  sexprs <- many prnt
  return (PrintBang sexprs)
  
end = do {linenum; symb "end"; return End}

if' :: Parser Sexpr
if' = do
  linenum
  symb "if"
  b <- comparison
  symb "then"
  n <- many $ sat isDigit
  return (If (b, read n))

goSub = do
  ln <- linenum
  symb "gosub"
  n <- many $ sat isDigit
  return (GoSub ln (read n))

goto = do
  ln <- linenum
  symb "goto"
  n <- many $ sat isDigit
  return (Goto (read n))

for = do
  n <- linenum
  symb "for"
  c <- token $ letter
  token $ char '='
  (Number f) <- expr
  symb "to"
  (Number l) <- expr
  return (For c (floor f) (floor l) 0)

next = do
  linenum
  symb "next"
  c <- letter
  return (Next c)

return' = do
  linenum
  symb "return"
  return (Return)

tab = do
  symb "tab"
  char '('
  n <- many number
  token $ char ')'
  let t = [' '| _ <- [1..(read n::Int)]]
  return (Symbol t)
  
--Eval--------------------------------------

eval :: Sexpr -> ReaderT Environment IO Sexpr
eval sexpr@(Input (c, q)) = do
  (table, _,_,_) <- ask
  liftIO $ putStrLn q
  v <- liftIO $ getLine
  liftIO $ writeArray table c (Symbol v)
  --liftIO $ putStrLn v
  return Void

eval sexpr@(Symbol s) = return sexpr 

eval Void = return Void
--  return (Symbol "Failed")

eval sexpr@(Let (c, s)) = do
  (table,_,_,_) <- ask
  liftIO $ writeArray table c s
  return Void

eval sexpr@(Variable c) = do
  (table, _,_,_) <- ask
  v <- liftIO $ readArray table c
  liftIO $ putStrLn $ (show v)
  return Void


eval (Prints []) = do {liftIO $ putStr $ "\n"; return Void}
eval sexpr@(Prints (s:ss)) = do
  liftIO $ putStr $ (show s)
  eval (Prints ss)
  --return Void

eval sexpr@(If (p,e)) = do
  if p then do
    (_,_,map,_) <- ask
    l <- liftIO $ readArray map e
    return (Control l)
    else do return Void

eval sexpr@(GoSub ln to) = do
  (_,_,map,ref) <- ask
  liftIO $ writeIORef ref sexpr
  l <- liftIO $ readArray map to
  return (Control l)

eval sexpr@(Goto ln) = do
  (_,_,map,_) <- ask
  l <- liftIO $ readArray map ln
  return (Control l)
  
eval sexpr@(For c f t l) = return sexpr

eval sexpr@(Next c) = do
  (table,_,_,_) <- ask
  (For _ f t l) <- liftIO $ readArray table c
  if f >= t then return Void else do
    --liftIO . putStrLn $ "Next"
    liftIO $ writeArray table c (For c (f+1) t l)
    return (Control l)

eval sexpr@Return = do
  (_,_,lineMap,r) <- ask
  (GoSub n _) <- liftIO $ readIORef r
  ln <- liftIO $ readArray lineMap n
  return (Control (ln+1))

eval (PrintBang []) = return Void
eval sexpr@(PrintBang (s:ss)) = do
  liftIO $ putStr $ (show s)
  eval (PrintBang ss)

eval End = return End

eval Failed = return Failed

checkEof :: Handle -> IO Bool
checkEof h = do
  if h == stdin then return False else hIsEOF h

basic :: Int -> ReaderT Environment IO ()
basic n = do
    env@(sTable, prog, lineMap, fors) <- ask
    line <- liftIO $ readArray prog n
    if line == "quit" then return () else do
      s <- liftIO $ parse' line env
      e <- eval s
      case e of
        (Control l) -> basic l
        (For c f t l) -> do
          --ln <- liftIO $ readArray lineMap 
          liftIO $ writeArray sTable c (For c f t (n+1))
          basic (n+1)
        Failed -> do {liftIO . putStrLn $ (show e); return ()}
        End -> return ()
        _ -> basic (n+1)       

getNum :: String -> String
getNum (x:xs) = if isDigit x then x:getNum xs else ""
  

loadProgram h ln pairs prog = do
  eof <- liftIO $ checkEof h
  if eof then return () else do
    line@(x:xs) <- hGetLine h
    let n = (read (getNum xs)::Int)
    liftIO $ writeArray pairs n ln
    liftIO $ writeArray prog ln line
    loadProgram h (ln+1) pairs prog
    
main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  arr1 <- newArray ('a', 'z') Void :: IO (IOArray Char Sexpr) --symbol table
  arr2 <- newArray (1,500) "" :: IO (IOArray Int String) --interp line # -> prog lines
  arr3 <- newArray (1,500) 500 :: IO (IOArray Int Int) --prog line # to interp line # mapping
  ref <- newIORef Void :: IO (IORef Sexpr)
  let env = (arr1, arr2, arr3, ref)
  args <- getArgs
  case args of
    [fName] -> do
      handle <- openFile fName ReadMode
      hGetLine handle --Read the (define foo'( 
      loadProgram handle 1 arr3 arr2
      writeArray arr2 500 "(500 end)))"
      runReaderT (basic 1) env
    _ -> do
      putStrLn "Failed to read Basic program input"
      return ()
