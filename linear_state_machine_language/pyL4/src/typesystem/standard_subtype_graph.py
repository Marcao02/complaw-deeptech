from typesystem.Sorts import *
from typesystem.reducers import build_graph

standard_types_graph = build_graph()

def sub(s1:Sort,s2:Sort) -> bool:
    if s1 == s2:
        return True
    if s1 == 'Any':
        print(f"warning: return true on check Any ⊆ {s2}")
        return True
    return standard_types_graph.hasEdge(s1,s2)


for num1 in UnboundedNumericSorts:
    for num2 in UnboundedNumericSorts:
        if not sub(num1,num2):
            continue
        for den1 in [PosReal,PosInt]:
            for den2 in [PosReal,PosInt]:
                if not sub(den1,den2):
                    continue
                standard_types_graph.addEdge(NonatomicSort('Rate',(num1,den1)), NonatomicSort('Rate',(num2,den2)))

for S in AllAtomicSorts:
    for T in AllAtomicSorts:
        if S == T or not sub(S,T):
            continue
        standard_types_graph.addEdge(NonatomicSort('Tuple',(S,S)), NonatomicSort('Tuple',(T,T)))

print("\n" + str(standard_types_graph))