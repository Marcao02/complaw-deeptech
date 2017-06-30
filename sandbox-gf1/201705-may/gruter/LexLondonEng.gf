instance LexLondonEng of LexLondon =
  LexGruterEng **
  open SyntaxEng, ParadigmsEng, IrregEng, ExtraEng
  in {
  oper
    L_win_V = mkV "win";
    L_win_exp = mkCl (mkNP the_Det (mkN "winning")) (mkA "winsome");
}
      