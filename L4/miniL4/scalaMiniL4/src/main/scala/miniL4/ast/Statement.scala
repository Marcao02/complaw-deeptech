package miniL4.ast


import miniL4.Name

object Statement {
  type Block = List[Statement]
}
import Statement.Block

abstract sealed class Statement(loc: Loc) extends ASTNode(loc) {}

  // recall TypeAnnotation is used if you want to assign a type to a local var
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

  // mostly for testing purposes
  case class AssertTypeError(stmt:Statement, loc:Loc = NoLoc) extends Statement(loc)