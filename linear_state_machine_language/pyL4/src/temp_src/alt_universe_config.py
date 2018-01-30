from src.model.Sort import Sort
from src.independent.typing_imports import *

class AltUniverseConfig(NamedTuple):
    name: str
    sort_copies: Dict[str,Sort]

"""
For SAFE and KISS, I THINK just need to simultaneously substitute:
SApp('Dup',NonnegReal,'$'), for SApp('Dup',NonnegReal,'jVar')  

Lighter notation...
In the following order. Substitute simultaneously even within sort op apps:
NonnegReal[$] for NonnegReal[jVar]  
PosReal[Pos$] for PosReal[jVar] 
Nat[Shares] for Nat[jVar] 
PosInt[PosShares] for PosInt[jVar]

Then this???
Ratio(Shares,Pos$)[Shares/$] for Ratio(Nat[jVar],PosReal[jVar])
Or do we just want to *define*
Shares/$ := Ratio(Shares,Pos$) ??  
"""

