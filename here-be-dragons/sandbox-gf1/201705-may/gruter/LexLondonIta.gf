instance LexLondonIta of LexLondon =
  LexGruterIta **
  open SyntaxIta, ParadigmsIta, IrregIta, ExtraIta
  in {
  oper
    L_win_V = mkV "win";
    L_win_exp = mkCl (mkNP the_Det (mkN "winning")) (mkA "winsome");
}
      
