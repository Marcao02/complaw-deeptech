--# -path=.:present:/Users/mengwong/non-db-src/l/compiler/sandbox-gf1/201705-may/gruter

instance LexMissouriEng of LexMissouri =
  LexGruterEng **
  open SyntaxEng, ParadigmsEng, IrregEng, ExtraEng
  in {
  oper
    L_win_V = mkV "win";
    L_win_exp = mkCl (mkNP the_Det (mkN "winning")) (mkA "winsome");
}
      