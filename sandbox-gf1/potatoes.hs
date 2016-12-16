

import System.Environment (getArgs)
import Control.Applicative
import Data.Char (isAlpha)
import Data.Map (Map)
import qualified Data.Map as M
import Text.Trifecta
import Debug.Trace

data Spheroid = Potato | Apple
                deriving (Eq, Ord, Show)
    
data Bag = Bag { contains :: Maybe Spheroid
               , count :: Maybe Integer }
           deriving (Eq, Ord, Show)

data Human = Human String
              deriving (Eq, Ord, Show)

data BagFact = BagFact { human :: Human
                       , bag :: Bag }
               deriving (Eq, Ord, Show)

parseBagNum   :: Parser (Maybe Integer)
parseBagNum = Just <$> decimal <|> pure Nothing
  
parseBag :: Parser Bag
parseBag = do
  numthings <- parseBagNum
  skipWhitespace;
  thingies <- some letter;
  char '.';
  skipEOL;
  let thingtype = case thingies of
                    "Potatoes" -> Potato
                    "Potato"   -> Potato
                    "Apple"    -> Apple
                    "Apples"   -> Apple
  return $ (Bag (Just thingtype) numthings)
                        
parseBagline :: Parser BagFact
parseBagline = do
  entity <- some letter;
  traceM ("parseBagline: found entity " ++ entity)
  string " has a Bag with ";
  bag <- parseBag
  return $ BagFact (Human entity) bag

parseEmptyBag :: Parser BagFact
parseEmptyBag = do
  entity <- some letter;
  traceM ("parseEmptyBag: found entity " ++ entity)
  string " has a Bag.";
  skipEOL;
  return $ BagFact (Human entity) (Bag Nothing Nothing)

skipEOL :: Parser ()
skipEOL = skipMany (oneOf "\n")
          
skipWhitespace :: Parser ()
skipWhitespace = skipMany (oneOf " \t")

main :: IO ()
main = do
  getArgs >>= mapM_ lumpy

lumpy arg = do
  r <- parseFromFile (many (try parseBagline <|> parseEmptyBag)) arg
  case r of
    Nothing -> return ()
    Just rs -> putStrLn (show rs)

-- rs now contains the AST of the potato world.
-- next step: use GF to produce the original text.

