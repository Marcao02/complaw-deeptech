package ast

abstract sealed class TermBinder {}
case class LetInBinder(src:LetIn) extends TermBinder
case class StateVarBinder(src:StateVarDef) extends TermBinder

abstract sealed class TermBinderO {}
case class LetInBinderO(src:LetIn) extends TermBinderO
case class StateVarBinderO(src:StateVarDef) extends TermBinderO
case object NoBinder extends TermBinderO