package ast

abstract sealed class Statement(loc: Loc) extends ASTNode(loc) {}

  case class LetIn(defs: Seq[(Name, Term)], block: Block, loc: Loc = NoLoc) extends Statement(loc)

  case class StateVarAssign(name: Name, rhs: Term, loc: Loc = NoLoc) extends Statement(loc) {
    def defn(link: ContractLinking): TermBinderO = {
      link.stateVarDefs.get(this.name) match {
        case None => NoBinder
        case Some(svd) => StateVarBinderO(svd)
      }
    }
  }

  case class IfElse(test: Term, trueBranch: Block, falseBranch: Block, loc: Loc = NoLoc) extends Statement(loc)