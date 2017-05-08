abstract Shall = {
  flags startcat = Contract ;
  cat
    Contract ; Party ; Deontic ; Action ;
  --  Deadline ; -- let's leave out the temporal for now
  fun
    Pred : Party -> Deontic -> Action -> Contract ;
    Shall, MustNot, May : Deontic ;    -- at some point switch this to Polarity
    Alice, Bob, Carol : Party ;
    Pay, ShipGoods : Action ;
}

