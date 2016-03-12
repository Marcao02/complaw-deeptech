module Katas where

import Legalese
-- Kata One

-- If the Company proposes to undertake an issuance of New Securities, it shall give notice to each Major Shareholder of its intention to issue New Securities (the "Notice") describing the type of New Securities and the price and the general terms upon which the Company proposes to issue the New Securities.

isMajor (Person "alice") = True
isMajor  _               = False

holders = [ Person "alice", Person "bob" ]

myco = Person "Company"

kataOne holders security = Happens [Event myco (Resolution (Intend [Event myco $ Issue security]))]
          :=>:
          Must myco
               (Happens [Event myco $
                         Send   x $
                         Intend [Event myco $ Issue security]
                          | x <- holders, isMajor x ])
  
