instance LexGruterEng of LexGruter =
  open SyntaxEng, ParadigmsEng, IrregEng, ExtraEng in
  {
  oper
    P_desirable  = mkA "desirable";

    P_default_act = mkVP (mkV2 "perform") (mkNP a_Det (mkN "action"));
    P_default_exp = mkCl (mkNP the_Det (mkN "action")) P_desirable;
    
    -- https://groups.google.com/forum/#!topic/gf-dev/O4z1lh2u0v4
    P_by_default = SyntaxEng.mkAdv by8means_Prep (mkNP (mkN "default"));


--    P_Always = SyntaxEng.mkAdv   if_Subj (mkS (mkCl P_the_sun  P_shines));
    
    where_Subj = mkSubj "where";
}
