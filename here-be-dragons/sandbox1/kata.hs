{-# LANGUAGE OverloadedStrings #-}
module Katas where

import Legalese

-- setup for the katas
holders :: [Person]
holders = [ Person "alice", Person "bob", Person "carol" ]

-- is someone a major investor?
isMajor :: Person -> Bool
isMajor (Person "alice") = True
isMajor  _               = False

-- there is a company
myco :: Person
myco = Person "Company"

-- Kata Example

-- If the Company proposes to undertake an issuance of New Securities, it shall give notice to each Major Shareholder of its intention to issue New Securities (the "Notice") describing the type of New Securities and the price and the general terms upon which the Company proposes to issue the New Securities.


kataOrig :: [Person] -> FinancialInstrument -> Person -> Integer -> Statement
kataOrig holders security y z =
  Happens [
      Event myco
    $ Resolution
    $ Intend [Event myco $ Issue y security z]]
          :=>:
          Must myco
               (Happens [Event myco $
                         Send   x $
                         Intend [Event myco $ Issue y security z]
                          | x <- holders, isMajor x ])

-- var kataOrig = function(holders, security, y, z) {
--     this.happens([(new Event(myco))(new Resolution)(new Intend)
--                   .....
--                  }..


-- "procures" means
-- Person "Alice" `Must` Happens [Event "AliceCompany"... ]

-- Statement: Alice must pay ten dollars to Bob.
-- Optimization: Alice must pay Bob ten dollars. If not, ...
-- Implication: If Alice fails to pay Bob ten dollars, ...
kata_conception = "Alice"
  `Must`
  Happens [Event "Alice" Pay { rcpt = "Bob"
                             , money = Money { amount=10.0
                                             , currency="SGD"
                                             }
                             }
          ]


main = do putStrLn $ asSpec kata_conception
          putStrLn $ asSpec kata_newborn


kata_newborn =
       ("Investor" `Means` "Alice")
  :&&: ("Company"  `Means` "Bob")
  :&&: ("Investor" `Must`
  Happens [Event "Alice" Pay { rcpt = "Bob"
                             , money = Money { amount=10.0
                                             , currency="SGD"
                                             }
                             }
          ])
  :&&: ("Company" `Must`
  Happens [Event "Company"
           $ Issue { rcpt     = "Investor"
                   , security = Shares Common
                   , quantity = 10 }
          ])

  where
    investor = "Alice"
    company  = "Bob"
-- The Company means Bob.
-- Investor means Alice.
-- The Investor will pay ten dollars to the Company.
-- The Company will issue to the Investor ten shares of Common Stock.

