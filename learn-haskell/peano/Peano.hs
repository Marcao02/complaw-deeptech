
{-# LANGUAGE GADTs #-}

data PeanoWire = Peano String
               | PList [PeanoWire]
               deriving (Show,Read)

data P  a where
  P  :: a     -> P a
  PL :: [P a] -> P a

instance (Show a) => Show (P a) where
  show (P   a) = "P "  ++ show a
  show (PL pa) = "PL " ++ show pa

main = do
  let p1 = Peano "Moo"
      p2 = PList [p1]
      p3 = PList [p1, PList [p1], PList [ PList [p1,p1]]]
  putStrLn $ show p3

  let q1 = P "foo"
      q2 = PL [P "moo", PL [P "poop"]]
  putStrLn $ show q2
  
  let r1 = P 1
      r2 = P 2
      r3 = PL [r1, r2]
  putStrLn $ show r3
