{-
CS 456 Basic Interpreter - Nicholas Spurlock

This program accepts lines of basic (In scheme list form) and interprets them into haskell
Right now it works on sample programs up to and including hamarabi.bas

This turned out to be partially a basic parser, but the lines between parser and interpreter are
blurred.  Because of this the program is not in a very traditionally functional form but mimics
an imperative language (perhaps that's ok because we're interpreting one).

There's plenty of room for improvement.  The unary and binary functions could definately be handled
better, ideally as a list of touples/triples where the function symbol could be parsed and used to
look up the actual function.  I briefly played with this, but didn't have the time to refactor things to get it to work.

Also, Parslib has been hijacked.  Most of the parsing library is the same, but I've added a data type Sexpr that holds the basic types, and an environment I pass around.  Also, the parser type has been changed.  I did this to allow for variable lookup during parsing. This seemed more efficient than essentially parsing an expression twice, but the price I payed was that everything is IO (probably not worth it in the long run, we loose out on the functional advantages).

I'm not sure the games work correctly.  Looking at the code for guess, it always seems to choose the number 2 unless your range is 1.  Also,
the math for the bushels doesn't work correctly so the numbers are off. Looking at the site for basic games, these games have been modified (presumably to check functionality) and the math doesn't seem to work all the time.
-}


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

index' :: Parser (Char, Int)
index' = do
  f:l <- token $ many alphanum
  if l == [] then return (f, 0) else return (f, (read l::Int))
  
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
  i <- index'
  s <- liftIO $ readArray table i
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

--unary :: Parser Sexpr --This could also be pattern matched
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

--This should be pattern matched
eq = do
  token $ char '('
  (Number e1) <- expr
  symb "="
  (Number e2) <- expr
  token $ char ')'
  let b = ((==) e1 e2)
  return b
gt = do
  token $ char '('
  (Number e1) <- expr
  symb ">"
  (Number e2) <- expr
  token $ char ')'
  let b = ((>) e1 e2)
  return b
lt = do
  token $ char '('
  (Number e1) <- expr
  symb "<"
  (Number e2) <- expr
  token $ char ')'
  let b = ((<) e1 e2)
  return b
gtEq = do
  token $ char '('
  (Number e1) <- expr
  symb ">="
  (Number e2) <- expr
  token $ char ')'
  let b = ((>=) e1 e2)
  return b
ltEq = do
  token $ char '('
  (Number e1) <- expr
  symb "<="
  (Number e2) <- expr
  token $ char ')'
  let b = ((<=) e1 e2)
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
  i <- index'
  char ')'  
  return (Input (i, q))

let' :: Parser Sexpr
let' = do
  linenum
  symb "let"
  i <- index'
  token $ char '='
  s <- many item
  let s' = take ((length s) - 1) s
  put s'
  e <- expr
  return (Let (i, e))

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
  i <- index'
  token $ char '='
  (Number f) <- expr
  symb "to"
  (Number l) <- expr
  return (For i (floor f) (floor l) 0)

next = do
  linenum
  symb "next"
  i <- index'
  return (Next i)

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
eval sexpr@(Input (i, q)) = do
  (table, _,_,_) <- ask
  liftIO $ putStrLn q
  v <- liftIO $ getLine
  liftIO $ writeArray table i (Symbol v)
  return Void

eval sexpr@(Symbol s) = return sexpr 

eval Void = return Void
--  return (Symbol "Failed")

eval sexpr@(Let (i, s)) = do
  (table,_,_,_) <- ask
  liftIO $ writeArray table i s
  --putVar c s table
  return Void

eval sexpr@(Variable i) = do
  (table, _,_,_) <- ask
  v <- liftIO $ readArray table i
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

eval sexpr@(Next i) = do
  (table,_,_,_) <- ask
  (For _ f t l) <- liftIO $ readArray table i
  if f >= t then return Void else do
    --liftIO . putStrLn $ "Next"
    liftIO $ writeArray table i (For i (f+1) t l)
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
  arr1 <- newArray (('a',0), ('z',25)) Void :: IO (IOArray (Char, Int) Sexpr) --symbol table
  arr2 <- newArray (1,10000) "" :: IO (IOArray Int String) --interp line # -> prog lines - This could just be a normal array
  arr3 <- newArray (1,10000) 10000 :: IO (IOArray Int Int) --prog line # to interp line # mapping - could also be a normal array
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


{- Examples
    foo.bas
λ> :main "foo.bas"
Test
What is A
1
What is B
4
What is C
1
12.0

     quadratic1.bas
λ> :main "quadratic1.bas"
What is the value of A
1
What is the value of B
4
What is the value of C
1
The 1st root is: -0.2679491
The 2nd root is: -3.732051

      quadratic2.bas
λ> :main "quadratic2.bas"
What is the value of A
1
What is the value of B
5
What is the value of C
1
The 1st root is: -0.2087121
The 2nd root is: -4.791288

λ> :main "quadratic2.bas"
What is the value of A
1
What is the value of B
1
What is the value of C
1
Imaginary roots.

    guess.bas
λ> :main "guess.bas"
                                 GUESS
               CREATIVE COMPUTING MORRISTOWN, NEW JERSEY

THIS IS A NUMBER GUESSING GAME. I'LL THINK
OF A NUMBER BETWEEN 1 AND ANY LIMIT YOU WANT.
THEN YOU HAVE TO GUESS WHAT IT IS.

WHAT LIMIT DO YOU WANT
25

I'M THINKING OF A NUMBER BETWEEN 1 AND 25.0

WHAT IS YOUR GUESS
23

TOO HIGH. TRY A SMALLER ANSWER.

WHAT IS YOUR GUESS
24

TOO HIGH. TRY A SMALLER ANSWER.

WHAT IS YOUR GUESS
1

TOO LOW. TRY A BIGGER ANSWER.

WHAT IS YOUR GUESS
4

TOO HIGH. TRY A SMALLER ANSWER.

WHAT IS YOUR GUESS
2

THAT'S IT! YOU GOT IT IN 5.0 TRIES.
YOU SHOULD HAVE BEEN ABLE TO GET IT IN ONLY 2.0 TRIES.

λ> :main "guess.bas"
                                 GUESS
               CREATIVE COMPUTING MORRISTOWN, NEW JERSEY

THIS IS A NUMBER GUESSING GAME. I'LL THINK
OF A NUMBER BETWEEN 1 AND ANY LIMIT YOU WANT.
THEN YOU HAVE TO GUESS WHAT IT IS.

WHAT LIMIT DO YOU WANT
5

I'M THINKING OF A NUMBER BETWEEN 1 AND 5.0

WHAT IS YOUR GUESS
-3

ILLEGAL VALUE.

WHAT IS YOUR GUESS
3

TOO HIGH. TRY A SMALLER ANSWER.

WHAT IS YOUR GUESS
1

TOO LOW. TRY A BIGGER ANSWER.

WHAT IS YOUR GUESS
2

THAT'S IT! YOU GOT IT IN 3.0 TRIES.
YOU SHOULD HAVE BEEN ABLE TO GET IT IN ONLY 2.0 TRIES

       hamurabi.bas
λ> :main "hamurabi.bas"
HAMURABI: Game of Hamurabi - Version 1.01

Corona Data Systems, Inc.

HAMURABI - 
WHERE YOU GOVERN THE ANCIENT KINGDOM OF SUMERIA.
THE OBJECT IS TO FIGURE OUT HOW THE GAME WORKS!!
IF YOU WANT TO QUIT, SELL ALL YOUR LAND.

HAMURABI, I BEG TO REPORT THAT LAST YEAR 0.0 PEOPLE
STARVED AND 5.0 PEOPLE CAME TO THE CITY.
THE POPULATION IS NOW 100.0.
WE HARVESTED 3000.0 BUSHELS AT 3.0 BUSHELS PER ACRE.
RATS DESTROYED 200.0 BUSHELS, LEAVING 2800.0 BUSHELS
IN THE STOREHOUSES.
THE CITY OWNS 1000.0 ACRES OF LAND.
LAND IS WORTH 23.0 BUSHELS PER ACRE.

HAMURABI...

HOW MANY ACRES DO YOU WISH TO BUY
50

HOW MANY ACRES DO YOU WISH TO SELL
0

HOW MANY BUSHELS SHALL WE DISTRIBUTE AS FOOD
200

HOW MANY ACRES SHALL WE PLANT
50


HAMURABI, I BEG TO REPORT THAT LAST YEAR 90.0 PEOPLE
STARVED AND -2.0 PEOPLE CAME TO THE CITY.
THE POPULATION IS NOW 8.0.
WE HARVESTED 300.0 BUSHELS AT 6.0 BUSHELS PER ACRE.
RATS DESTROYED 120.0 BUSHELS, LEAVING 1605.0 BUSHELS
IN THE STOREHOUSES.
THE CITY OWNS 1050.0 ACRES OF LAND.
LAND IS WORTH 23.0 BUSHELS PER ACRE.

HAMURABI...

HOW MANY ACRES DO YOU WISH TO BUY
50

HOW MANY ACRES DO YOU WISH TO SELL
25

HOW MANY BUSHELS SHALL WE DISTRIBUTE AS FOOD
200

HOW MANY ACRES SHALL WE PLANT
100

HAMURABI, THINK AGAIN - 
YOU ONLY HAVE 8.0 PEOPLE, 1075.0 ACRES, AND 
830.0 BUSHELS IN STOREHOUSES.
HOW MANY ACRES SHALL WE PLANT
5


HAMURABI, I BEG TO REPORT THAT LAST YEAR 0.0 PEOPLE
STARVED AND 0.0 PEOPLE CAME TO THE CITY.
THE POPULATION IS NOW 8.0.
WE HARVESTED 30.0 BUSHELS AT 6.0 BUSHELS PER ACRE.
RATS DESTROYED 60.0 BUSHELS, LEAVING 798.0 BUSHELS
IN THE STOREHOUSES.
THE CITY OWNS 1075.0 ACRES OF LAND.
LAND IS WORTH 23.0 BUSHELS PER ACRE.

HAMURABI...

HOW MANY ACRES DO YOU WISH TO BUY
0

HOW MANY ACRES DO YOU WISH TO SELL
0

HOW MANY BUSHELS SHALL WE DISTRIBUTE AS FOOD
500

HOW MANY ACRES SHALL WE PLANT
0


HAMURABI, I BEG TO REPORT THAT LAST YEAR 0.0 PEOPLE
STARVED AND 9.0 PEOPLE CAME TO THE CITY.
THE POPULATION IS NOW 17.0.
WE HARVESTED 0.0 BUSHELS AT 6.0 BUSHELS PER ACRE.
RATS DESTROYED 20.0 BUSHELS, LEAVING 278.0 BUSHELS
IN THE STOREHOUSES.
THE CITY OWNS 1075.0 ACRES OF LAND.
LAND IS WORTH 23.0 BUSHELS PER ACRE.

HAMURABI...

HOW MANY ACRES DO YOU WISH TO BUY
0

HOW MANY ACRES DO YOU WISH TO SELL
0

HOW MANY BUSHELS SHALL WE DISTRIBUTE AS FOOD
0

HOW MANY ACRES SHALL WE PLANT
0


HAMURABI, I BEG TO REPORT THAT LAST YEAR 17.0 PEOPLE
STARVED AND 0.0 PEOPLE CAME TO THE CITY.
THE POPULATION IS NOW 0.0.
WE HARVESTED 0.0 BUSHELS AT 6.0 BUSHELS PER ACRE.
RATS DESTROYED 19.0 BUSHELS, LEAVING 259.0 BUSHELS
IN THE STOREHOUSES.
THE CITY OWNS 1075.0 ACRES OF LAND.
LAND IS WORTH 23.0 BUSHELS PER ACRE.

HAMURABI...

HOW MANY ACRES DO YOU WISH TO BUY

-}
