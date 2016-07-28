module Security where

import Data.List.Split
import Data.List
  
commafy x = h++t
    where
        sp = break (== '.') $ show x
        h = reverse $ intercalate "," $ chunksOf 3 $ reverse $ fst sp
        t = snd sp

