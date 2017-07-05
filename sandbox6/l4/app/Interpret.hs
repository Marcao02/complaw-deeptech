
-- based on TestBurger.hs


module Main where


import System.IO ( stdin, hGetContents )
import System.Environment ( getArgs, getProgName )
import System.Exit ( exitFailure, exitSuccess )
import Control.Monad (when)

import LexL
import ParL
import SkelL
import PrintL
import AbsL
import LayoutL

import Interpreter


import ErrM

type ParseFun a = [Token] -> Err a

myLLexer = resolveLayout True . myLexer

type Verbosity = Int

putStrV :: Verbosity -> String -> IO ()
putStrV v s = when (v > 1) $ putStrLn s

runFile :: (Print a, Show a) => Verbosity -> ParseFun a -> FilePath -> IO ()
runFile v p f = putStrLn f >> readFile f >>= run v p

runFile2 :: Verbosity -> ParseFun L4Module -> FilePath -> IO ()
runFile2 v p f = readFile f >>= run2 v p

run :: (Print a, Show a) => Verbosity -> ParseFun a -> String -> IO ()
run v p s = let ts = myLLexer s in case p ts of
           Bad s    -> do putStrLn "\nParse              Failed...\n"
                          putStrV v "Tokens:"
                          putStrV v $ show ts
                          putStrLn s
                          exitFailure
           Ok  tree -> do putStrLn "\nParse Successful!"
                          showTree v tree
                          exitSuccess

run2 :: Verbosity -> ParseFun L4Module -> String -> IO ()
run2 v p s = let lexed = myLLexer s
                 parsed = p lexed
             in case parsed of
                  Ok l4module -> putStrLn $ unlines $ interpret l4module
                                 

showTree :: (Show a, Print a) => Int -> a -> IO ()
showTree v tree
 = do
--      putStrV v $ "\n[Abstract Syntax]\n\n" ++ show tree
      putStrV v $ "\n[Linearized tree]\n\n" ++ printTree tree

usage :: IO ()
usage = do
  putStrLn $ unlines
    [ "usage: Call with one of the following argument combinations:"
    , "  --help          Display this help message."
    , "  (no arguments)  Parse stdin verbosely."
    , "  (files)         Parse content of files verbosely."
    , "  -s (files)      Silent mode. Parse content of files silently."
    ]
  exitFailure

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--help"] -> usage
    [] -> getContents >>= run 2 pL4Module
    "-s":fs -> mapM_ (runFile 0 pL4Module) fs
    "-v":fs -> mapM_ (runFile 0 pL4Module) fs
    fs -> mapM_ (runFile2 2 pL4Module) fs

          



