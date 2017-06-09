instance LexPartyEng of LexParty =
  open SyntaxEng, ParadigmsEng, IrregEng, ExtraEng
  in {
  oper
    -- if you want the party name construction to be different for different languages,
    -- feel free to define "partyname" differently -- the default is optional.
    partyname = defaultpartyname;
}
