instance LexSizeEng of LexSize = open SyntaxEng, StructuralEng, ParadigmsEng in {
  oper
    sml2A sml = ( case sml.size of {
      Small => mkA "small";
      Medium => mkA "medium";
      Large => mkA "large"
      } );
}
