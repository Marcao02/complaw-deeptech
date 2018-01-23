"""
given a map N typ -> Set[name],
and   a map I typ -> Set[ind]
will return a set of map C of typ to ____, such that for each typ:
    C[typ] is the set |N[typ]|*|I[typ]| combinations of elements of N[typ]
"""