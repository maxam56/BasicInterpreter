{---------------------------------------------------------------------

           A HASKELL LIBRARY OF MONADIC PARSER COMBINATORS

                          17th April 1997

               Graham Hutton              Erik Meijer
          University of Nottingham   University of Utrecht

This Haskell 1.3 library is derived from our forthcoming JFP article
"Monadic Parsing in Haskell".  The library also includes a few extra
combinators that were not discussed in the article for reasons of space:

   o force (used to make "many" deliver results lazily);

   o digit, lower, upper, letter, alphanum (useful parsers);

   o ident, nat, int (useful token parsers).

---------------------------------------------------------------------}

module Parselib
   (Parser, Environment, Sexpr(..), item, sat, (+++), string, many, many1, sepby, sepby1,
    chainl, chainl1, char, digit, lower, upper, letter, alphanum,
    symb, ident, nat, int, token, parse, space, integer, natural) where

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
import Control.Applicative hiding (many)

infixr 5 +++

-- Monad of parsers: -------------------------------------------------
type Parser a = ReaderT Environment (StateT String IO) a
type Environment = (IOArray Char String, IOArray Int String, IOArray Int Int, IOArray Char Sexpr)

data Sexpr = Symbol String | Variable Char | Number Float | Boolean Bool | Input (Char, String) | Let (Char, String) | Prints [Sexpr] | If (Bool, Int)  | GoSub Int | For Char Int Int Int| Next Char | Control Int | Void | Failed | End

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
  show (Let (c, i)) = (show c) ++ "=" ++ i

parse p = runStateT p

item            :: Parser Char
item             = do {(c:cs) <- get; put cs; return c}
                           
sat             :: (Char -> Bool) -> Parser Char
sat p            = do {c <- item; if p c then return c else mzero}

-- Efficiency improving combinators: ---------------------------------

--force           :: Parser a -> Parser a
--force p          = Parser (\cs -> let xs = parse p cs in
--                              (fst (head xs), snd (head xs)) : tail xs)

(+++)           :: Parser a -> Parser a -> Parser a
p +++ q          = p `mplus` q

-- Recursion combinators: --------------------------------------------

string          :: String -> Parser String
string ""        = return ""
string (c:cs)    = do {char c; string cs; return (c:cs)}

many            :: Parser a -> Parser [a]
many p           = many1 p +++ return mzero

many1           :: Parser a -> Parser [a]
many1 p          = do {a <- p; as <- many p; return (a:as)}

sepby           :: Parser a -> Parser b -> Parser [a]
p `sepby` sep    = (p `sepby1` sep) +++ return []

sepby1          :: Parser a -> Parser b -> Parser [a]
p `sepby1` sep   = do {a <- p; as <- many (do {sep; p}); return (a:as)}

chainl          :: Parser a -> Parser (a -> a -> a) -> a -> Parser a
chainl p op a    = (p `chainl1` op) +++ return a

chainl1         :: Parser a -> Parser (a -> a -> a) -> Parser a
p `chainl1` op   = do {a <- p; rest a}
                   where
                      rest a = do {f <- op; b <- p; rest (f a b)}
                               +++ return a

-- Useful parsers: ---------------------------------------------------

char            :: Char -> Parser Char
char c           = sat (c ==)

digit           :: Parser Int
digit            = do {c <- sat isDigit; return (ord c - ord '0')}

lower           :: Parser Char
lower            = sat isLower

upper           :: Parser Char
upper            = sat isUpper

letter          :: Parser Char
letter           = sat isAlpha

alphanum        :: Parser Char
alphanum         = sat isAlphaNum

symb            :: String -> Parser String
symb cs          = token (string cs)

ident           :: [String] -> Parser String
ident css        = do cs <- token identifier
                      guard (not (elem cs css))
                      return cs

identifier      :: Parser String
identifier       = do {c <- lower; cs <- many alphanum; return (c:cs)}

nat             :: Parser Int
nat              = token natural

natural         :: Parser Int
natural          = digit `chainl1` return (\m n -> 10*m + n)

int             :: Parser Int
int              = token integer

integer         :: Parser Int
integer          = do {char '-'; n <- natural; return (-n)} +++ nat

-- Lexical combinators: ----------------------------------------------

space           :: Parser String
space            = many (sat isSpace)

token           :: Parser a -> Parser a
token p          = do {a <- p; space; return a}
