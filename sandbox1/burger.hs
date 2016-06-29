#!/usr/local/bin/runhaskell

-- let's pretend we have a simple BNF
-- dinner ::= burger beer
-- burger ::= "bun" "meat"+ "cheese"* "lettuce"* "tomato"*
-- beer   ::= "beer"

-- how do we represent this in haskell?

import Data.List as List
import Data.Map.Lazy as Map

import Control.Arrow
import Text.Printf

uniq :: (Eq a, Ord a) => [a] -> [(Int, a)]
uniq = List.map (length &&& head) . group . sort

uniq_c :: [String] -> [String]
uniq_c l =
    let us = uniq $ l
        width = length . show . maximum . List.map fst $ us
    in  List.map (uncurry $ printf "%*d %s" width) us


-- Prelude Map Data.List> mapAccumL (\k a -> (Map.insert a ((Map.findWithDefault 0 a k)+1) k, a+0)) Map.empty [1,2,3,4,1,1,1,2]
-- (fromList [(1,4),(2,2),(3,1),(4,1)],[1,2,3,4,1,1,1,2])

data DinnerE = Dinner BurgerE BeerE
             deriving (Show)

data BeerE   = Lager | Ale
             deriving (Show)
data BunE    = PlainBun | SesameBun | CornBun
             deriving (Show)
data MeatE   = Chicken | Beef
             deriving (Show)
data ToppingE = Cheese | Lettuce | Tomato
             deriving (Show)

data BurgerE = Burger BunE MeatEs [ToppingE]
             deriving (Show)

data MeatEs = MeatEs [MeatE]
              deriving (Show)

class English a where
  english :: a -> String

instance English BeerE where
  english Lager = "top-fermented beer"
  english Ale   = "bottom-fermented beer"
  
instance English BunE where
  english PlainBun = "plain bun"
  english SesameBun = "sesame seed bun"
  english CornBun = "gluten free bun"

instance English MeatE where
  english Chicken = "ground processed chicken product patty"
  english Beef = "all-beef patty"

instance English MeatEs where
  english (MeatEs a) = commaAnd $ uniq_c $ List.map english a

instance English ToppingE where
  english Cheese = "slice of Gouda"
  english Lettuce = "tasteless greens"
  english Tomato = "tasteless factory tomato"

instance English BurgerE where
  english (Burger a b c) = ("tasty "
                            ++ english b
                            ++ " lovingly wrapped in a "
                            ++ english a
                            ++ " topped with ")
    ++ (commaAnd $ uniq_c $ List.map english c)




-- one would hope that this sort of rule is available in GenI
-- otherwise there would just be too much reinvention of wheel

commaAnd :: [String] -> String
commaAnd (    y:[]) = y
commaAnd (  x:y:[]) =              x ++  " and " ++ y
commaAnd (w:x:y:[]) = w ++ ", " ++ x ++ ", and " ++ y
commaAnd (    x:xs) = x ++ ", " ++ commaAnd xs

