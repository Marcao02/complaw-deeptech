package miniL4

import miniL4.ast.{ASTNode, BullshitASTNode}

class L4ProjectError(msg:String) extends Error(msg)
  class BugInCodebase(msg:String) extends L4ProjectError(msg)

  class L4SrcError(msg:String, node:ASTNode = BullshitASTNode) extends L4ProjectError(msg)


    class TypeError(msg: String, node:ASTNode = BullshitASTNode) extends L4SrcError(msg,node)

    class EvalError(msg: String, node:ASTNode = BullshitASTNode) extends L4SrcError(msg,node)