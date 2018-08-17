package object miniL4 {
  val CONTRACT_ROLE = 'Contract
  def bugassert(test:Boolean, msg:String) : Unit = if(!test) throw new BugInCodebase(msg) else ()
}
