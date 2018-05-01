from typing import Union,List,Set
import parse_sexpr_mult_parens_then_mult_commas
from l4fnc_util import streqci

parse_all_generic = parse_sexpr_mult_parens_then_mult_commas.parse_all
to_sexpr = parse_sexpr_mult_parens_then_mult_commas.to_sexpr_tuple
pretty  = parse_sexpr_mult_parens_then_mult_commas.pretty_generic

class ASTNode:
	def __init__(self, sexpr):
		self.sexpr = sexpr

class TopL4FNS(ASTNode):
	def __init__(self, sexpr):
		super().__init__(sexpr)
		self.global_vars = None
		self.claims = None
		self.participants = None
		self.prose_contract = None
		self.code_contract = None

	def assemble(l,sexpr):
		# print(l[0])
		toplevel = TopL4FNS(sexpr)

		for x in l:		
			if len(x) >= 2:
				x0,x1 = x[0],x[1]
				# print(x)
				if streqci(x0,'global') and streqci(x1,'variables'):
					toplevel.global_vars = GlobalVarsBlock.assemble(x[2:],x)
				elif streqci(x0,'claims'):
					toplevel.claims = ClaimsBlock.assemble(x[1:],x)
				elif streqci(x0,'participants'):
					toplevel.participants = ParticipantsBlock.assemble(x[1:],x)
				elif streqci(x0,'prose') and streqci(x1,'contract'):
					toplevel.prose_contract = ProseContract.assemble(x[1:],x)
				elif streqci(x0,'formal') and streqci(x1,'contract'):
					toplevel.code_contract = CodeContract.assemble(x[1:],x)

		return toplevel					
		
class GlobalVarsBlock(ASTNode):
	def assemble(l,sexpr):
		pass

class GlobalVarDecl(ASTNode):
	def __init__(self,name:str,sort:str,initval:Union[str,int]):
		self.name = None
		self.sort = None
		self.initval = None

	def assemble(l,sexpr)
		
class ClaimsBlock(ASTNode):
	def assemble(l,sexpr):
		pass

class ClaimDecl(ASTNode):
	pass		

class ParticipantsBlock(ASTNode):
	def assemble(l,sexpr):
		pass

class ProseContract(ASTNode):
	def assemble(l,sexpr):
		return "the prose contract part"

class CodeContract(ASTNode):	
	def assemble(l,sexpr):
		return "the formal contract part"

FILES = (
    'printer.L4fnc',
)

if __name__ == '__main__':
    import sys
    if 'test' in sys.argv:
        import doctest
        doctest.testmod()

    if 'file' in sys.argv:        
        for path in FILES:
            print("\nLooking at file " + path + ":\n")
            fil = open(path,'r')            
            parsed = parse_all_generic(fil.read())
            print(parsed)
            print(pretty(parsed))
            # sexpr = to_sexpr(parsed)
            # print(sexpr)
            # print( parseToplevel(sexpr) )
            
