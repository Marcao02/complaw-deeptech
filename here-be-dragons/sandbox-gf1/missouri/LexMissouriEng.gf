--# -path=.:present

instance LexMissouriEng of LexMissouri =
  LexGruterEng **
  open SyntaxEng, ParadigmsEng, IrregEng, ExtraEng
  in {
  oper
    L_win_V = mkV "win";
    L_win_exp = mkCl (mkNP the_Det (mkN "winning")) (mkA "winsome");
}
      