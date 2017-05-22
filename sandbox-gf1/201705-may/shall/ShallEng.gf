concrete ShallEng of Shall = {
  lincat
     Clause, Party, Deontic, Action = { s : Str } ;
  lin
     Pred p d a = {s= p.s ++ d.s ++ a.s };
     Must       = {s="shall"};
     MustNot    = {s="must not"};
     May        = {s="may"};
     Alice      = {s="Alice"};
     Bob        = {s="Bob"};
     Carol      = {s="Carol"};
     Pay        = {s="pay"};
     ShipGoods  = {s="ship goods"};
}
