import System.IO
import System.Environment
import Control.Monad
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.State hiding (get, put)
import Control.Monad.Trans.Writer hiding (tell)
import Control.Monad.Trans.List
import Control.Monad.IO.Class
--import Control.Monad.State.Class hiding (get, put)
import Control.Monad.Writer.Class
import Data.Char

type Parser a = StateT String (MaybeT IO) a

--parse s = runStateT (runWriterT (input s)) ""

item :: Parser Char
item = do
  (x:xs) <- get
  put xs
  return x

char :: Char -> Parser Char
char c = sat (c==)

sat :: (Char -> Bool) -> Parser Char
sat p = do
  c <- item
  if p c then return c else mzero

string :: String -> Parser String
string [] = return ""
string (c:cs) = do {char c; string cs; return (c:cs)}

many :: Parser a -> Parser [a]
many p = many1 p `mplus` mzero

many1 :: Parser a -> Parser [a]
many1 p = do {a <- p; as <- many p; return (a:as)}

querry :: Parser (IO ())
querry = do
  s <- many (sat (/= '"'))
  return (putStrLn s)

spaces :: Parser String
spaces = many (sat isSpace)

token :: Parser String
token = many (sat isAlphaNum)


--interpret :: Parser (IO ())
--interpret = input `mplus` mzero
  

input :: Parser [(String, IO String)]
input = do
  char '('
  lnNum <- many (sat isDigit)
  spaces
  string "input"
  spaces
  q <- querry
  spaces
  key <- token
  next <- input
  return ((key, getLine):next)


--mainLoop :: Show a => String -> Maybe a
--mainLoop s = runStateT interpret s
  
--main = do
--  args <- getArgs
--  case args of
--    [fName] -> do
--      handle <- openFile fName ReadMode
--      contents <- hGetContents handle
--      case (mainLoop contents) of
--        Just a -> do {a}
--        Nothing -> do {putStrLn "Failed"}
--      hClose handle
--    _ -> putStrLn "No input file specified!"
  
