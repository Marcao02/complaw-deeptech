{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE PackageImports      #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeOperators       #-}

module Main where

import "base" Control.Applicative ((<$>), (<*>), (<*))
import "base" Control.Exception (try)
import "base" Control.Monad (join, (>=>))
import "mtl" Control.Monad.Error (ErrorT(ErrorT, runErrorT), throwError)
import "mtl" Control.Monad.Reader (ReaderT(ReaderT, runReaderT), asks)
import "mtl" Control.Monad.Trans (lift, liftIO)
import "transformers" Control.Monad.Trans.Error (Error(..))
import "bifunctors" Data.Bifunctor (Bifunctor(..))
import "base" Data.Char (toUpper)
import "compdata" Data.Comp (Cxt(Term), stripA, Term)
import "compdata" Data.Comp.Ops ((:+:)(Inl,Inr))
import "transformers" Data.Functor.Identity (Identity(Identity, runIdentity))
import "containers" Data.Map (empty, singleton, keys, elems)
import "base" Data.Maybe (catMaybes)
import "base" Data.Monoid ((<>))
import "time" Data.Time (LocalTime(LocalTime), midnight, getCurrentTime, utctDay, Day)
import "time" Data.Time.Calendar (fromGregorian)
import "base" GHC.IO.Exception (IOException)
import "optparse-applicative" Options.Applicative (strOption, short, metavar, helper, info, execParser, fullDesc)
import "poets" Poets.Contracts.Base (Contract(..), ContractMetaData)
import "poets" Poets.Contracts.Language.CSL (CSLError(..), ContractDefCorePos, builtInPredefined, Closure(..), subTypeRel, ContractDef(..), ContractDefCore, FunctionDef(..), ClauseDef(..))
import "poets" Poets.Contracts.Language.CSL.AST (RecEnv, Breach, Breach(..), ExprCore, CoreClause(..))
import "poets" Poets.Contracts.Language.CSL.AST.Type (TypeUnit, TypeVar, TypeFunction)
import "poets" Poets.Contracts.Language.CSL.Analysis.Concludable (concludable)
import "poets" Poets.Contracts.Language.CSL.Desugar (desugarContractDef, desugarExpr)
import "poets" Poets.Contracts.Language.CSL.Interpreter (instantiate, stepC)
import "poets" Poets.Contracts.Language.CSL.Parser (parseExpr)
import qualified "poets" Poets.Contracts.Language.CSL.Parser as CSL (parseContract)
import "poets" Poets.Contracts.Language.CSL.Render (closureToDoc, clauseToDoc)
import "poets" Poets.Contracts.Language.CSL.TypeChecker (typeCheckContractDefinition)
import "poets" Poets.Data.Type (POETSRecordEnv, newRecordEnv, TypeConstant, TypeEnt, TypeList)
import "poets" Poets.Data.Type.Parser (parsePCE)
import "poets" Poets.Data.Value (Val(..), VEntity(..), VRecord(..), VFields, VField(..), RecordName, DateTime, newFields, fieldsList, iVDateTime, iVString, iVRecord)
import "base" System.Exit (exitWith, ExitCode(..))
import "base" System.IO (stdout, hSetBuffering, BufferMode(..))
import "parsec" Text.Parsec (ParsecT, Stream, digit, char, parse)

data CLIFlags = CLIFlags { contract :: FilePath, ontology :: FilePath }

getCLIFlags :: IO CLIFlags
getCLIFlags = execParser $ info (helper <*> parseCLIFlags) fullDesc where
  parseCLIFlags = CLIFlags
    <$> (strOption $ short 'c' <> metavar "contract.csl")
    <*> (strOption $ short 'o' <> metavar "ontology.pce")

data Return = Fulfillment | POETSError CSLError

instance Show Return where
  show Fulfillment = "The contract was fulfilled!"
  show (POETSError err) = show err

instance Error Return where
  strMsg = POETSError . RunTimeError

type M = ErrorT Return (ReaderT CLIFlags IO)

liftIdentity :: Monad m => ErrorT e (ReaderT r Identity) a -> ErrorT e (ReaderT r m) a
liftIdentity = ErrorT . ReaderT . fmap (return . runIdentity) . runReaderT . runErrorT

type N m a = ErrorT Return (ReaderT RecEnv m) a

runM :: M a -> (CLIFlags -> IO a)
runM comp flags = runReaderT (runErrorT comp) flags >>= \res -> case res of
  Left (POETSError er) -> print er >> exitWith (ExitFailure 1)
  Left Fulfillment -> print Fulfillment >> exitWith ExitSuccess
  Right _ -> exitWith ExitSuccess

(<$$>) :: Functor f => Functor g => (a -> b) -> f (g a) -> f (g b)
(<$$>) = fmap fmap fmap ; infixl 4 <$$>

(<$$$>) :: Functor f => Functor g => Functor h => (a -> b) -> f (g (h a)) -> f (g (h b))
(<$$$>) = fmap fmap $ fmap fmap fmap ; infixl 4 <$$$>

type File = (FilePath, String)

readFileWithName :: FilePath -> M File
readFileWithName path = ErrorT . lift $ do
  bimap (strMsg . (show :: IOException -> String)) (path,) <$> try (readFile path)

parseOntology :: File -> POETSRecordEnv -> Either Return POETSRecordEnv
parseOntology = first (POETSError . ParseError) <$$> uncurry parsePCE

parseOntologies :: [File] -> Either Return POETSRecordEnv
parseOntologies = foldr (\path env -> env >>= parseOntology path) (Right $ newRecordEnv [])

promoteOntology :: Term (TypeConstant :+: (TypeList :+: TypeEnt))
                -> Term (TypeConstant :+: (TypeList :+: (TypeEnt :+: (TypeUnit :+: (TypeVar :+: TypeFunction)))))
promoteOntology (Term (Inl x)) = Term (Inl (fmap promoteOntology x))
promoteOntology (Term (Inr (Inl x))) = Term (Inr (Inl (fmap promoteOntology x)))
promoteOntology (Term (Inr (Inr x))) = Term (Inr (Inr (Inl (fmap promoteOntology x))))

getOntology :: M RecEnv
getOntology = asks ontology >>= readFileWithName >>= liftE . (promoteOntology <$$$> parseOntologies) . return

parseContract :: File -> Either Return ContractDefCorePos
parseContract = bimap (POETSError . ParseError) desugarContractDef . uncurry CSL.parseContract

liftE :: Monad m => Either a b -> ErrorT a m b
liftE = ErrorT . return

getContract :: M ContractDefCorePos
getContract = asks contract >>= readFileWithName >>= liftE . parseContract


typecheck :: RecEnv -> ContractDefCorePos -> M ()
typecheck ontology = liftE . first POETSError . typeCheckContractDefinition ontology empty (snd builtInPredefined)

stripPos :: ContractDefCorePos -> ContractDefCore
stripPos def@ContractDefinition{..} = def
  { contractDefFunDefs = fmap (\def -> def { functionExp = stripA (functionExp def) }) contractDefFunDefs
  , contractDefClauseDefs = fmap (\def -> def { clauseDefBody = stripA (clauseDefBody def) }) contractDefClauseDefs
  , contractDefBody = stripA contractDefBody
  }

checkBreach :: (a -> M b) -> Either Breach a -> M b
checkBreach = either $ \(Breach ts party) -> throwError (POETSError $ ContractBreach ts party)

instantiateContract :: RecEnv -> ContractDefCorePos -> M (Contract Closure)
instantiateContract ontology csl = do
  startDate <- liftIO (flip LocalTime midnight . utctDay <$> getCurrentTime)
  rdClosure <- liftE . first POETSError $ instantiate
    (subTypeRel ontology)
    (contractDefName csl `singleton` stripPos csl)
    (Term . VRecord $ VR
       { vrecordName = contractDefType csl
       , vrecordFields = newFields $
         [ VF "contractStartDate" (iVDateTime startDate)
         , VF "templateName" (iVString $ contractDefName csl)
         , VF "startDate" (iVDateTime startDate)
         ]
       })
    0
  checkBreach return rdClosure

--------------------------------------------------------------------------------

data Event = Event { timestamp :: DateTime, action :: Term Val }

invertVf :: VFields (Either a b) -> Either a (VFields b)
invertVf = newFields <$$> mapM (\VF{..} -> second (VF vfieldName) vfieldValue) . fieldsList

exprToValM :: ExprCore -> Either Return (Term Val)
exprToValM (Term (Inl (k :: Val ExprCore))) = case k of
  (VInt i) -> Right $ Term (VInt i)
  (VBool b) -> Right $ Term (VBool b)
  (VString s) -> Right $ Term (VString s)
  (VDate d) -> Right $ Term (VDate d)
  (VTime t) -> Right $ Term (VTime t)
  (VDateTime dt) -> Right $ Term (VDateTime dt)
  (VDuration du) -> Right $ Term (VDuration du)
  (VReal db) -> Right $ Term (VReal db)
  (VRecord e) -> Term . VRecord . VR (vrecordName e) <$> invertVf (exprToValM <$> vrecordFields e)
  (VEnt en) -> Right $ Term (VEnt en)
  (VList xs) -> Term . VList <$> mapM exprToValM xs
exprToValM (Term (Inr _)) = throwError . POETSError . RunTimeError $ "Expected a value, got an expression."

parseValue :: String -> Either Return (Term Val)
parseValue = join . bimap (POETSError . ParseError) (exprToValM . stripA . desugarExpr) . parseExpr []

digits :: (Stream s m Char, Integral n) => Int -> ParsecT s u m n
digits n = fromIntegral . foldr (+) 0 <$> (mapM (\n -> ((*n) . read . return) <$> digit) (take n $ iterate (`div` 10) (10 ^ (n - 1))))

parseDay :: Stream s m Char => ParsecT s u m Day
parseDay = fromGregorian <$> (digits 4 <* char '-') <*> (digits 2 <* char '-') <*> digits 2

parseDateTime :: Stream s m Char => ParsecT s u m DateTime
parseDateTime = flip LocalTime midnight <$> parseDay

parseEvent :: String -> Either Return Event
parseEvent input = let (date, tx) = fmap tail (splitAt 10 input) in Event
  <$> first (POETSError . ParseError) (parse parseDateTime [] date)
  <*> parseValue tx

getEvent :: M Event
getEvent = do
  liftIO $ hSetBuffering stdout NoBuffering >> putStr "Transaction: "
  liftIO getLine >>= either (liftIO . print >=> const getEvent) return . parseEvent

--------------------------------------------------------------------------------

bool :: a -> a -> (Bool -> a)
bool a _ True = a
bool _ a False = a

attemptConclusion :: RecEnv -> Contract Closure -> M ()
attemptConclusion environment state =
  (liftE . first POETSError $
    concludable (subTypeRel environment) [] (fst builtInPredefined) state) >>=
  checkBreach (throwError Fulfillment `bool` return ())

execREPL :: RecEnv -> Contract Closure -> M (Contract Closure)
execREPL environment state = do
  liftIO . print . clauseToDoc . closureClause . contractContent $ state
  attemptConclusion environment state
  Event{..} <- getEvent
  residue <- liftE . bimap POETSError snd $
    stepC (subTypeRel environment) undefined (fst builtInPredefined) timestamp action state
  flip checkBreach residue $ \clause -> execREPL environment $ state
    { contractContent = (contractContent state) { closureClause = clause }
    , contractLastUpdate = timestamp
    }

main' :: M (Contract Closure)
main' = do
  (csl, ont) <- (,) <$> getContract <*> getOntology
  typecheck ont csl
  instantiateContract ont csl >>= execREPL ont

main :: IO (Contract Closure)
main = getCLIFlags >>= runM main'
