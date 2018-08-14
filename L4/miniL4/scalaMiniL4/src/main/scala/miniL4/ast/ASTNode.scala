package miniL4.ast

class ASTNode(loc:Loc) {
  def parentToplevelNode(linking: ContractLinking) : ToplevelNode = this match {
    case tln:ToplevelNode => tln
    case _ => linking.parentNode(this).parentToplevelNode(linking)
  }
}

object BullshitASTNode extends ASTNode(NoLoc)