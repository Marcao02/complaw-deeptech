
import logging
from typing import List, Union, Optional, NamedTuple
from constants_and_defined_types import GlobalVarId
from parse_sexpr import SExpr, SExprOrStr

class Security:
    name: str
    allSecurities: [Security]
    
    def convert(self) -> Security:
    
class ConvertibleSecurity(Security):
    cap: Optional[int]
    discount: Optional[float]


    
class SAFE(ConvertibleSecurity):

    def capitalization(self) -> int:
        """
        "Company Capitalization” means the sum, as of immediately prior to the Equity Financing, of: (1) all shares of Capital Stock (on an as-converted basis) issued and outstanding, assuming exercise or conversion of all outstanding vested and unvested options, *warrants* and other convertible securities, but *excluding* (A) this instrument, (B) all other Safes, and (C) *convertible promissory notes*; and (2) all shares of Common Stock reserved and available for future grant under any equity incentive or similar plan of the Company, and/or any equity incentive or similar plan to be created or increased in connection with the Equity Financing.
        """
    
class SAFE_nocap_nodiscount(SAFE):
    
class SAFE_cap_nodiscount(SAFE):
    
class SAFE_cap_discount(SAFE):
    
class SAFE_nocap_discount(SAFE):
    
class KISS(ConvertibleSecurity):

    def capitalization(self) -> int:
        """
        “Fully-Diluted Capitalization” shall mean the number of shares of outstanding Common Stock of the Company on a fully-diluted basis, *including* (i) conversion or exercise of all securities convertible into or exercisable for Common Stock, (ii) exercise of all outstanding options and *warrants* to purchase Common Stock and, in the case of Section 1(b)(i) and 1(b)(iii) only, (iii) the shares reserved or authorized for issuance under the Company’s existing stock option plan or any stock option plan created or increased in connection with such transaction; but excluding, for this purpose, the conversion contemplated by the applicable provision of Section 2.
        """

    
class KISS_debt(KISS):

class KISS_equity(KISS):

class Shares(Security):
    preMoneyValuation: int
    pricePerShare: int
    
class OrdinaryShares(Shares):

class PreferredShares(ConvertibleSecurity,Shares):

    
    


# 
# dustin i got (and there’s a decent chance this is a bit off):
# ss = X · rs · [ (1 + rk)/(1 - rk·rs) ]
# where 
# ss is the number of shares allocated to the SAFE investor
# X is the number from before, of projected non-KISS non-SAFE shares
# rs is (investment of SAFE investor / price that SAFE investor gets)  
# rk is (investment of KISS investor / price that KISS investor gets)
# 
# [17:10] 
# oh and symmetrically 
# ks = X · rk · [ (1 + rs)/(1 - rk·rs) ]
# 



    
