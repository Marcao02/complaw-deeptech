package miniL4

import indy.sexpr.{LocatedSExpr, SExpr}
import miniL4.ast.{ASTNode, BullshitASTNode}

class L4ProjectError(msg:String) extends Error(msg)
  class BugInCodebase(msg:String) extends L4ProjectError(msg)

  class ParseError(msg: String, expr:LocatedSExpr) extends L4ProjectError(msg) {
//    def this(expr:LocatedSExpr) = this(s"Parse error in file ${expr.filePath} in the range ${expr.lcspan.left} to ${expr.lcspan.right}.", expr)
//    def this(expr:LocatedSExpr, msg:String) = this(s"${msg}\nFile ${expr.filePath}, range ${expr.lcspan.left} to ${expr.lcspan.right}.", expr)
    def this(expr:LocatedSExpr) = this(s"Parse error. See ${expr.loc}.", expr)
    def this(expr:LocatedSExpr, msg:String) = this(s"Parse error. See ${expr.loc}.", expr)
  }

  class L4SrcError(msg:String, node:ASTNode = BullshitASTNode) extends L4ProjectError(msg)

    class TypeError(msg: String, node:ASTNode = BullshitASTNode) extends L4SrcError(msg,node)

    class EvalError(msg: String, node:ASTNode = BullshitASTNode) extends L4SrcError(msg,node)
