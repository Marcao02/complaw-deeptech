
-- based on TestBurger.hs


module Main where


import System.IO ( stdin, hGetContents )
import System.Environment ( getArgs, getProgName )
import System.Exit ( exitFailure, exitSuccess )
import Control.Monad (when)

import LexL
import ParL
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
                  Ok l4module -> interpret l4module
                  otherwise   -> putStrLn "Parse failed. re-run with -v"



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
    [] -> getContents >>= run2 0 pL4Module
    "-s":fs -> mapM_ (runFile 2 pL4Module) fs
    "-v":fs -> mapM_ (runFile 2 pL4Module) fs
    fs -> mapM_ (runFile2 0 pL4Module) fs


-- usage: ast "examples/burger1.l4"
ast :: String -> IO L4Module
ast f = readFile f >>= run3 pL4Module


run3 :: ParseFun a -> String -> IO a
run3 p s = do
  let lexed = myLLexer s
      parsed = p lexed
  return $ case parsed of
    Ok l4module -> l4module
    Bad s       -> error ("Parse failed. " ++ s)


{-- in ghci

> let myast = ast "examples/burger1.l4"
> (MkL4Module sections) <- myast
> [concat <$> transSection] <*> sections

<interactive>:44:2: error:
    • No instance for (Foldable Err) arising from a use of ‘concat’
    • In the first argument of ‘(<$>)’, namely ‘concat’
      In the expression: concat <$> transSection
      In the first argument of ‘(<*>)’, namely
        ‘[concat <$> transSection]’
> transSection <$> sections
[Ok ["sectionimport"],Ok ["sectioncontract"],Ok ["sectionparties"],Ok ["sectiondefine"],Ok ["sectiontypes"],Ok ["sectioncl"],Ok ["sectioncl"],Ok ["sectioncl"],Ok ["sectioncl"],Ok ["sectioncl"],Ok ["sectionaction"],Ok ["sectionaction"],Ok ["sectionaction"],Ok ["sectionaction"],Ok ["sectionaction"]]
> map unerr $ transSection <$> sections
["sectionimport","sectioncontract","sectionparties","sectiondefine","sectiontypes","sectioncl","sectioncl","sectioncl","sectioncl","sectioncl","sectionaction","sectionaction","sectionaction","sectionaction","sectionaction"]
--}
