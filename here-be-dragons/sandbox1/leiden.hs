dashList mydepth prefix lang a =
  concat
  $ fmap (++ "\n")
  $ fmap (((concat (take mydepth $ repeat " ")) ++ prefix ++ " ") ++)
  $ fmap lang a


data Temporal = At     Whenever
              | Upon   Whenever
              | When   Whenever
              | While  Whenever
              | Before Whenever
              | After  Whenever
              | From   Whenever
          deriving (Show)
instance English Temporal where
  en_SG (At     whenever) = "Temporal At " ++ whenever
  en_SG (Upon   whenever) = "Temporal Upon " ++ whenever
  en_SG (When   whenever) = "Temporal When " ++ whenever
  en_SG (While  whenever) = "Temporal While " ++ whenever
  en_SG (Before whenever) = "Temporal Before " ++ whenever
  en_SG (After  whenever) = "Temporal After " ++ whenever
  en_SG (From   whenever) = "Temporal From " ++ whenever
