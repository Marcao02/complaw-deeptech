
{-# LANGUAGE DeriveGeneric, DeriveAnyClass, OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Main where

-- mengwong: i am making progress with my haskell differ.
-- what is the haskell differ? it reads JSON snapshots of
-- before & after company state, including cap table of
-- holders and holdings, and it calculates the diffs between
-- those snapshots, much as
-- https://github.com/thsutton/aeson-diff does. then it uses
-- a simple set of logic rules implemented in native Haskell
-- to calculate the state transitions needed to go from one
-- state to the other. the output: an ordered list of
-- resolutions and agreements.

-- for aeson
import GHC.Generics
import Data.Aeson
import Data.ByteString.Lazy as BSL
import Data.ByteString.Lazy.Char8 as Char8

-- for option parsing
import Options.Applicative
import Data.Semigroup ((<>))
import Control.Monad

-- for the company data model
import Company
import CDiff
import BusinessLogic

-- various utilities
import Data.Maybe (catMaybes, fromMaybe)
import Data.Map as Map
import Data.List

-- in the future, get aeson-schema to automatically produce even this parser

{-                       OPTION MANAGEMENT                    -}
{-                       OPTION MANAGEMENT                    -}
{-                       OPTION MANAGEMENT                    -}
-- https://hackage.haskell.org/package/optparse-applicative#alternative
data Options = Options { quiet :: Bool
                       , verbose :: Bool
                       , beforefilename :: String
                       , afterfilename  :: String}

optparse :: Parser Options
optparse = Options -- in order defined above
  <$> switch ( long "quiet"   <> short 'q' <> help "quiet mode" )
  <*> switch ( long "verbose" <> short 'v' <> help "verbose mode" )
  <*> argument str (metavar "before.json")
  <*> argument str (metavar "after.json.")

main :: IO ()
main = main1 =<< execParser opts
       where opts = info (optparse <**> helper) ( fullDesc
                                                  <> progDesc "compare two company states"
                                                  <> header "companyDelta" )
         
{-                               MAIN                           -}
{-                               MAIN                           -}
{-                               MAIN                           -}

main1 :: Options -> IO ()
main1 opts@(Options quiet v beforefilename afterfilename) = do
  vprint v $ "Main runs! quiet = "  ++ show quiet ++     "; verbose = " ++ show v
  vprint v $ "Main runs! infile = " ++ beforefilename ++ "; outfile = " ++ afterfilename

  (before, after) <- parseJSONs opts

  -- diff the company

  let companydiffs = rdiff (company before) (company after)
  vprint v $ "companydiffs: " ++ show companydiffs

  -- diff the holdings

--  let hdiffs = pairBy holder holds (holdings before) (holdings after)
--  vprint v $ show $ assocs hdiffs

--  let reorganizedHoldings = [ rdiff (Holding hname (fromMaybe [] h1))
--                                    (Holding hname (fromMaybe [] h2))
--                            | (hname,(h1,h2)) <- assocs hdiffs
--                            ]
--  vprint v $ show reorganizedHoldings

  -- diff the full companystate
  
  vprint v $ show $ rdiff before after

  vprint v $ "now let's have a look at the business logic rules on the diffs"

  -- apply the diff->paperwork rules to get the first cut of dependencies
  let deps1 = applyDrules diffRules (rdiff before after)

  -- apply the paperwork -> paperwork rules to get the second cut of dependencies
  -- unfoldr :: (b -> Maybe (a, b)) -> b -> [a]
  -- b == [Paperwork]
  -- a == [Paperwork]
  -- unfoldr ([Paperwork] -> Maybe ([Paperwork], [Paperwork])) -> [Paperwork] -> [[Paperwork]]
  let deps2 = Data.List.unfoldr (applyPrules companydiffs paperRules) deps1
  vprint True $ show $ join [deps1, join deps2]


{-                               input json parsing                           -}

parseJSONs :: Options -> IO (CompanyState, CompanyState)
parseJSONs opts@(Options quiet v beforefilename afterfilename) = do
  beforefile <- BSL.readFile beforefilename
  vprint v $ "beforefile: read " ++ show (BSL.length beforefile) ++ " bytes of input"
  afterfile <- BSL.readFile afterfilename
  vprint v $ "afterfile: read " ++ show (BSL.length afterfile) ++ " bytes of input"

  let beforeJSON = eitherDecode beforefile :: Either String CompanyState
  vprint v $ case beforeJSON of
    (Left  parseError)  -> "beforeJSON error: "  ++ parseError
    (Right parseOutput) -> "beforeJSON output: " ++ show(parseOutput)

  let afterJSON = eitherDecode afterfile :: Either String CompanyState
  vprint v $ case afterJSON of
    (Left  parseError)  -> "afterJSON error: "  ++ parseError
    (Right parseOutput) -> "afterJSON output: " ++ show(parseOutput)

  case (beforeJSON, afterJSON) of
    (Right before, Right after) -> return (before, after)
    _                           -> fail "parse error. terminating."




{-                               utilities                           -}

-- verbose debug trace
vprint :: Bool -> String -> IO ()
vprint v s = when v $ Prelude.putStrLn s
