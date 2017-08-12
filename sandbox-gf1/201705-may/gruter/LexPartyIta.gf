instance LexPartyIta of LexParty =
  open SyntaxIta, ParadigmsIta, IrregIta, ExtraIta
  in {
  oper
    -- if you want the party name construction to be different for different languages,
    -- feel free to define "partyname" differently -- the default is optional.
    partyname = defaultpartyname;
}
