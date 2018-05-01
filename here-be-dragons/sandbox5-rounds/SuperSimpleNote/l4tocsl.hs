{-# LANGUAGE BangPatterns #-}
module Main where

import Control.Applicative
import Control.Exception (bracket)
import System.Environment (getArgs)
import System.IO (hClose, openFile, IOMode(ReadMode))
import Text.Trifecta
import Text.Parser.Token.Highlight
import Text.Parser.Token.Style
import Data.CharSet.ByteSet as S
import qualified Data.ByteString as B

infixl 4 <$!>

(<$!>) :: Monad m => (a -> b) -> m a -> m b
f <$!> ma = do
  a <- ma
  return $! f a

isHSpace :: Char -> Bool
isHSpace c = c == ' ' || c == '\t'

skipHSpaces :: CharParsing m => m ()
skipHSpaces = skipSome (satisfy isHSpace)

skipEOL :: Parser ()
skipEOL = skipMany (char '\n')
              
endOfLine :: CharParsing m => m ()
endOfLine = (string "\r\n" *> pure ()) <|> (char '\n' *> pure ())

lumpy arg = do
  r <- parseFromFile (many l4parser) arg
  case r of
    Nothing -> return ()
    Just rs -> do
              putStrLn ("parsed " ++ show (length rs) ++ " records")
              putStrLn (show rs)

main :: IO ()
-- main = mapM_ lumpy =<< getArgs
-- main = getArgs >>= mapM_ lumpy
main = do
  myargs <- getArgs
  mapM_ putStrLn (map ("1: handling " ++) myargs)
  mapM_ (putStrLn . ("2: handling " ++)) myargs
  mapM_ (\a -> putStrLn ("3: handling " ++ a)) myargs
  mapM_ lumpy myargs

{- output:
parsed 2 records
[(Request {requestMethod = "GET", requestUri = "http://slashdot.org/", requestProtocol = "1.1"},[Header {headerName = "foo", headerValue = ["this is a test"]}]),(Request {requestMethod = "GET", requestUri = "http://slashdot.org/", requestProtocol = "9..1.0.1"},[Header {headerName = "foo", headerValue = ["of the emergency broadcast system"]}])]

[(Request {requestMethod = "GET"
, requestUri = "http://slashdot.org/"
, requestProtocol = "1.1"}
,[Header {headerName = "foo"
, headerValue = ["this is a test"]}])
,
(Request {requestMethod = "GET",
requestUri = "http://slashdot.org/",
requestProtocol = "9..1.0.1"},
[Header {headerName = "foo",
headerValue = ["of the emergency broadcast system"]}])]
-}


{- transformations from L4 to CSL, organized by clause BNF -}

data L4E = L4Exp L4Exp
         | L4Data String String
         | L4Entity
         | L4Def
         | L4Comment String
         | L4Unknown String
           deriving (Eq, Ord, Show)

data L4Exp = L4Main String | L4C L4C
           deriving (Eq, Ord, Show)

data L4C = L4Or L4C L4C
         | L4And L4C L4C
         | L4Ob
         | L4ExternalChoice
         | L4InternalChoice
         | L4Fulfilment
           deriving (Eq, Ord, Show)

l4comment :: Parser L4E
l4comment = do
  string "--"
  commentstr <- many (noneOf "\n")
  skipEOL
  return $ L4Comment commentstr

l4data :: Parser L4E
l4data = do
  string "data "
  lhs <- token (some (noneOf "= ")); skipSome (oneOf "= ")
  rhs <- token (some (noneOf "\n")); skipEOL
  return $ L4Data lhs rhs
  
l4noncomment :: Parser L4E
l4noncomment = do
  input <- some (noneOf "\n")
  return $ L4Unknown input
         
l4parser :: Parser [L4E]
l4parser = many (l4comment <|> l4data <|> l4noncomment)
           <* some endOfLine
