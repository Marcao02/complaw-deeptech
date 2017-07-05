{-# LANGUAGE LambdaCase #-}


module Interpreter where
import AbsL
import PrintL
import Control.Monad
import ErrM

unerr :: Err [String] -> String
unerr = \case
        Ok  strs -> unlines strs
        Bad str  -> "*** " ++ str
    
interpret :: L4Module -> IO ()
interpret = putStrLn . unerr . transL4Module







-- ------------------ everything below this line originated in SkelL.hs                


type Result = Err [String]

failure :: Show a => a -> Result
failure x = Bad $ "Undefined case: " ++ show x

transIdent :: Ident -> Result
transIdent x = case x of
  Ident string -> failure x
transUIdent :: UIdent -> Result
transUIdent x = case x of
  UIdent string -> failure x
transL4Module :: L4Module -> Result
transL4Module x = case x of
  MkL4Module sections ->
      Ok ( ( "We have here an L4 Module, containing " ++ show (length sections) ++ " sections." ) : (map unerr $ transSection <$> sections) )
transSection :: Section -> Result
transSection x = case x of
  MkSectionImport sectionimport -> Ok ["sectionimport"]
  MkSectionContract sectioncontract -> Ok ["sectioncontract"]
  MkSectionParties sectionparties -> Ok ["sectionparties"]
  MkSectionDefine sectiondefine -> Ok ["sectiondefine"]
  MkSectionTypes sectiontypes -> Ok ["sectiontypes"]
  MkSectionClause sectioncl -> Ok ["sectioncl"]
  MkSectionAction sectionaction -> Ok ["sectionaction"]
transSectionImport :: SectionImport -> Result
transSectionImport x = case x of
  MkHeaderImport lingam bodyimports -> failure x
transBodyImport :: BodyImport -> Result
transBodyImport x = case x of
  MkBodyImport uident importsyms -> failure x
transImportSym :: ImportSym -> Result
transImportSym x = case x of
  MkImportSymI ident -> failure x
  MkImportSymU uident -> failure x
transLingam :: Lingam -> Result
transLingam x = case x of
  MkLingam -> failure x
transSectionContract :: SectionContract -> Result
transSectionContract x = case x of
  MkHeaderContract lingam defcontracts -> failure x
transDefContract :: DefContract -> Result
transDefContract x = case x of
  MkDefContract regulardef -> failure x
transSectionParties :: SectionParties -> Result
transSectionParties x = case x of
  MkHeaderParties lingam typeddefaliass -> failure x
transTypedDefAlias :: TypedDefAlias -> Result
transTypedDefAlias x = case x of
  MkTypedDefAlias ident uident aliasparen rbt -> failure x
transRBT :: RBT -> Result
transRBT x = case x of
  MkRBTblock recordblock -> failure x
  MkRBTterm terms -> failure x
transAliasParen :: AliasParen -> Result
transAliasParen x = case x of
  MkAliasParen aliasbodys -> failure x
transAliasBody :: AliasBody -> Result
transAliasBody x = case x of
  MkAliasBodyString regularstring -> failure x
  MkAliasBodySymbol regularsymbol -> failure x
transSectionDefine :: SectionDefine -> Result
transSectionDefine x = case x of
  MkHeaderDefine defineheader lingam typeddefaliass -> failure x
transDefineHeader :: DefineHeader -> Result
transDefineHeader x = case x of
  MkHeaderDefineText -> failure x
  MkHeaderDefineText2 -> failure x
transSectionTypes :: SectionTypes -> Result
transSectionTypes x = case x of
  MkHeaderTypes lingam deftypes -> failure x
transDefType :: DefType -> Result
transDefType x = case x of
  MkDefType uident sumtypes -> failure x
transSumType :: SumType -> Result
transSumType x = case x of
  MkSumTypeUIdent uident -> failure x
  MkSumTypeRecord recordblock -> failure x
transRecordBlock :: RecordBlock -> Result
transRecordBlock x = case x of
  MkRecordBlockExtension baseprefix recordblock -> failure x
  MkRecordBlock regulardefs -> failure x
transBasePrefix :: BasePrefix -> Result
transBasePrefix x = case x of
  MkRecordBasePrefix recordbases -> failure x
transRecordBase :: RecordBase -> Result
transRecordBase x = case x of
  MkRecordBase uident -> failure x
transRegularDef :: RegularDef -> Result
transRegularDef x = case x of
  MkRegularDefTerm ident term -> failure x
transRegularString :: RegularString -> Result
transRegularString x = case x of
  MkRegularString string -> failure x
transRegularSymbol :: RegularSymbol -> Result
transRegularSymbol x = case x of
  MkRegularSymbol uident -> failure x
transTerm :: Term -> Result
transTerm x = case x of
  MkTermUIdent uident -> failure x
  MkTermIdent ident -> failure x
  MkTermString string -> failure x
  MkTermFloat double -> failure x
  MkTermInteger integer -> failure x
  MkTermParens term -> failure x
transSectionCl :: SectionCl -> Result
transSectionCl x = case x of
  MkSectionClMustSequence cllabel clgiven clparty clmust cldeadline clmustconnector -> failure x
  MkSectionClMustSolo cllabel clgiven clparty clmust cldeadline clmusthence clmustlest -> failure x
  MkSectionClMay cllabel clgiven clparty clmay cldeadline clmaythen clmaylest clmayconnector -> failure x
transClLabel :: ClLabel -> Result
transClLabel x = case x of
  MkClLabel uident string -> failure x
transClGiven :: ClGiven -> Result
transClGiven x = case x of
  MkClGivenNone -> failure x
  MkClGiven givens -> failure x
transClGiving :: ClGiving -> Result
transClGiving x = case x of
  MkClGiving givens -> failure x
  NoClGiving -> failure x
transClParty :: ClParty -> Result
transClParty x = case x of
  MkClParty terms -> failure x
transClMay :: ClMay -> Result
transClMay x = case x of
  MkClMay mayword deonticaction -> failure x
  MkClMayNot mayword deonticaction -> failure x
transClMust :: ClMust -> Result
transClMust x = case x of
  MkClMust mustword deonticaction -> failure x
  MkClMustNot mustword deonticaction -> failure x
transMustWord :: MustWord -> Result
transMustWord x = case x of
  MustWord_MUST -> failure x
  MustWord_SHALL -> failure x
transMayWord :: MayWord -> Result
transMayWord x = case x of
  MayWord_MAY -> failure x
transDeadlineWord :: DeadlineWord -> Result
transDeadlineWord x = case x of
  DeadlineWord_WITHIN -> failure x
  DeadlineWord_BEFORE -> failure x
  DeadlineWord_DUE -> failure x
transClDeadline :: ClDeadline -> Result
transClDeadline x = case x of
  MkClDeadline deadlineword deadlinebody -> failure x
transDeadlineBody :: DeadlineBody -> Result
transDeadlineBody x = case x of
  DeadlineBody_immediately -> failure x
  DeadlineBody_IMMEDIATELY -> failure x
  DeadlineBody1 terms remainingorblank -> failure x
  DeadlineBody2 ident remainingorblank -> failure x
transRemainingOrBlank :: RemainingOrBlank -> Result
transRemainingOrBlank x = case x of
  MkRemainingOrBlank ident -> failure x
  MkRemainingBlank -> failure x
transClMayThen :: ClMayThen -> Result
transClMayThen x = case x of
  MkClMayThen thenword clausespec -> failure x
transClMayLest :: ClMayLest -> Result
transClMayLest x = case x of
  MkClMayLest lestword clausespec -> failure x
transClMayConnector :: ClMayConnector -> Result
transClMayConnector x = case x of
  MkClMayConnectorBlank -> failure x
  MkClMayConnectorClause clausespec -> failure x
transClauseSpec :: ClauseSpec -> Result
transClauseSpec x = case x of
  MkClauseSpecPlain uident -> failure x
  MkClauseSpec uident callargs -> failure x
transClMustHence :: ClMustHence -> Result
transClMustHence x = case x of
  MkClMustHenceBlank -> failure x
  MkClMustHence hencebody -> failure x
transClMustLest :: ClMustLest -> Result
transClMustLest x = case x of
  MkClMustLestBlank -> failure x
  MkClMustLest lestbody -> failure x
transHenceBody :: HenceBody -> Result
transHenceBody x = case x of
  MkHenceBodyF -> failure x
  MkHenceBodyCase ident casebodys -> failure x
transLestBody :: LestBody -> Result
transLestBody x = case x of
  MkLestBodySpec clausespec -> failure x
  MkLestBody -> failure x
transCaseBody :: CaseBody -> Result
transCaseBody x = case x of
  MkCaseBody terms casestatements -> failure x
transCaseStatement :: CaseStatement -> Result
transCaseStatement x = case x of
  MkCaseStmAss stmassignment -> failure x
  MkCaseStmCall clausespec -> failure x
  MkCaseStmF -> failure x
  MkCaseStmB -> failure x
  MkCaseStmNoop -> failure x
transStmAssignment :: StmAssignment -> Result
transStmAssignment x = case x of
  MkStmAssignment ident terms -> failure x
transClMustConnector :: ClMustConnector -> Result
transClMustConnector x = case x of
  MkClMustConnectorHence -> failure x
transThenWord :: ThenWord -> Result
transThenWord x = case x of
  ThenWord_THEN -> failure x
transHenceWord :: HenceWord -> Result
transHenceWord x = case x of
  HenceWord_HENCE -> failure x
transLestWord :: LestWord -> Result
transLestWord x = case x of
  LestWord_LEST -> failure x
transDeonticAction :: DeonticAction -> Result
transDeonticAction x = case x of
  MkDeonticAction uident callargs -> failure x
  MkDeonticAction1 deonticaction1 deonticaction2 -> failure x
  MkDeonticAction2 deonticaction1 deonticaction2 -> failure x
transCallArg :: CallArg -> Result
transCallArg x = case x of
  MkCallArgLeft ident terms -> failure x
  MkCallArgRight ident term -> failure x
  MkCallArgEquals ident terms -> failure x
transGiven :: Given -> Result
transGiven x = case x of
  MkGivenPlain givenscopes ident typeannotation givendefault -> failure x
transGivenDefault :: GivenDefault -> Result
transGivenDefault x = case x of
  MkGivenDefaultNone -> failure x
  MkGivenDefault terms -> failure x
transTypeAnnotation :: TypeAnnotation -> Result
transTypeAnnotation x = case x of
  MkTypeAnnotation typespecs -> failure x
  MkTypeAnnotationNone -> failure x
transTypeSpec :: TypeSpec -> Result
transTypeSpec x = case x of
  MkTypeSpec uident -> failure x
transGivenScope :: GivenScope -> Result
transGivenScope x = case x of
  MkGivenScope -> failure x
transSectionAction :: SectionAction -> Result
transSectionAction x = case x of
  MkHeaderSection uident clgiven clgiving actionbody -> failure x
transActionBody :: ActionBody -> Result
transActionBody x = case x of
  MkActionBodyNoop -> failure x
  MkActionBodyEvent string terms -> failure x
  MkActionBodyAss stmassignment -> failure x
  MkActionBodyMulti actionlines -> failure x
transActionLine :: ActionLine -> Result
transActionLine x = case x of
  MkActionLine actionbody -> failure x

