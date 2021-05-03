-- Copyright 2019 Google LLC
--
-- Use of this source code is governed by a BSD-style
-- license that can be found in the LICENSE file or at
-- https://developers.google.com/open-source/licenses/bsd

module Parser (Parser, parseit, parseProg, runTheParser, parseData,
               parseTopDeclRepl, uint, withSource, parseExpr, exprAsModule,
               emptyLines, brackets, symbol, symChar, keyWordStrs) where

import Control.Monad
import Control.Monad.Combinators.Expr
import Control.Monad.Reader
import Text.Megaparsec hiding (Label, State)
import Text.Megaparsec.Char hiding (space, eol)
import qualified Text.Megaparsec.Char as MC
import Data.Char (isLower)
import Data.Functor
import Data.List.NonEmpty (NonEmpty (..))
import Data.Void
import qualified Data.Set as S
import Data.String (fromString)
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Text.Megaparsec.Debug

import Name
import Syntax
import PPrint
import HigherKinded

type S = SourceNS

-- canPair is used for the ops (,) (|) (&) which should only appear inside
-- parentheses (to avoid conflicts with records and other syntax)
data ParseCtx = ParseCtx { curIndent :: Int
                         , canPair   :: Bool
                         , canBreak  :: Bool }
type Parser = ReaderT ParseCtx (Parsec Void String)

parseProg :: String -> [SourceBlock]
parseProg s = mustParseit s $ manyTill (sourceBlock <* outputLines) eof

parseData :: String -> Except (UExpr S)
parseData s = parseit s $ expr <* (optional eol >> eof)

parseTopDeclRepl :: String -> Maybe SourceBlock
parseTopDeclRepl s = case sbContents b of
  UnParseable True _ -> Nothing
  _ -> Just b
  where b = mustParseit s sourceBlock

parseExpr :: String -> Except (UExpr S)
parseExpr s = parseit s (expr <* eof)

parseit :: String -> Parser a -> Except a
parseit s p = case runTheParser s (p <* (optional eol >> eof)) of
  Left e -> throw ParseErr (errorBundlePretty e)
  Right x -> return x

mustParseit :: String -> Parser a -> a
mustParseit s p  = case parseit s p of
  Right ans -> ans
  Left e -> error $ "This shouldn't happen:\n" ++ pprint e

importModule :: Parser SourceBlock'
importModule = ImportModule <$> do
  keyWord ImportKW
  s <- (:) <$> letterChar <*> many alphaNumChar
  eol
  return s

sourceBlock :: Parser SourceBlock
sourceBlock = do
  offset <- getOffset
  pos <- getSourcePos
  (src, (level, b)) <- withSource $ withRecovery recover $ do
    level <- logLevel <|> logTime <|> logBench <|> return LogNothing
    b <- sourceBlock'
    return (level, b)
  return $ SourceBlock (unPos (sourceLine pos)) offset level src b Nothing

recover :: ParseError String Void -> Parser (LogLevel, SourceBlock')
recover e = do
  pos <- liftM statePosState getParserState
  reachedEOF <-  try (mayBreak sc >> eof >> return True)
             <|> return False
  consumeTillBreak
  let errmsg = errorBundlePretty (ParseErrorBundle (e :| []) pos)
  return (LogNothing, UnParseable reachedEOF errmsg)

consumeTillBreak :: Parser ()
consumeTillBreak = void $ manyTill anySingle $ eof <|> void (try (eol >> eol))

logLevel :: Parser LogLevel
logLevel = do
  void $ try $ lexeme $ char '%' >> string "passes"
  passes <- many passName
  eol
  case passes of
    [] -> return LogAll
    _ -> return $ LogPasses passes

logTime :: Parser LogLevel
logTime = do
  void $ try $ lexeme $ char '%' >> string "time"
  eol
  return PrintEvalTime

logBench :: Parser LogLevel
logBench = do
  void $ try $ lexeme $ char '%' >> string "bench"
  benchName <- stringLiteral
  eol
  return $ PrintBench benchName

passName :: Parser PassName
passName = choice [thisNameString s $> x | (s, x) <- passNames]

passNames :: [(String, PassName)]
passNames = [(show x, x) | x <- [minBound..maxBound]]

sourceBlock' :: Parser SourceBlock'
sourceBlock' =
      proseBlock
  <|> topLevelCommand
  <|> liftM declToModule (topDecl <* eolf)
  <|> liftM declToModule (instanceDef True  <* eolf)
  <|> liftM declToModule (instanceDef False <* eolf)
  <|> liftM declToModule (interfaceDef <* eolf)
  <|> liftM (Command (EvalExpr Printed) . exprAsModule) (expr <* eol)
  <|> hidden (some eol >> return EmptyLines)
  <|> hidden (sc >> eol >> return CommentLine)
  where
    declsToModule = RunModule . UModule . toNest
    declToModule = declsToModule . (:[])

proseBlock :: Parser SourceBlock'
proseBlock = label "prose block" $ char '\'' >> fmap (ProseBlock . fst) (withSource consumeTillBreak)

topLevelCommand :: Parser SourceBlock'
topLevelCommand =
      importModule
  <|> explicitCommand
  <?> "top-level command"

explicitCommand :: Parser SourceBlock'
explicitCommand = undefined
-- explicitCommand = do
--   cmdName <- char ':' >> nameString
--   cmd <- case cmdName of
--     "p"       -> return $ EvalExpr Printed
--     "t"       -> return $ GetType
--     "html"    -> return $ EvalExpr RenderHtml
--     "export"  -> ExportFun <$> nameString
--     _ -> fail $ "unrecognized command: " ++ show cmdName
--   e <- blockOrExpr <* eolf
--   return $ case (e, cmd) of
--     (WithSrcH _ (UVar (v:>())), GetType) -> GetNameType (asGlobal v)
--     _ -> Command cmd (exprAsModule e)

exprAsModule :: UExpr S -> (Name S, UModule S S)
exprAsModule = undefined
-- exprAsModule e = (asGlobal v, UModule (toNest [d]))
--   where
--     v = mkName "_ans_"
--     d = ULet PlainLet (WithSrcH (srcPos e) (nameToPat v), Nothing) e

-- === uexpr ===

expr :: Parser (UExpr S)
expr = mayNotPair $ makeExprParser leafExpr ops

-- expression without exposed infix operators
leafExpr :: Parser (UExpr S)
leafExpr = parens (mayPair $ makeExprParser leafExpr ops)
         <|> uTabCon
         <|> uVarOcc
         <|> uHole
         <|> uString
         <|> uLit
         <|> uPiType
         <|> uLamExpr
         <|> uViewExpr
         <|> uForExpr
         <|> caseExpr
         <|> ifExpr
         <|> uPrim
         <|> unitCon
         <|> (uLabeledExprs `fallBackTo` uVariantExpr)
         <|> uIsoSugar
         <|> uDoSugar
         <?> "expression"

containedExpr :: Parser (UExpr S)
containedExpr =   parens (mayPair $ makeExprParser leafExpr ops)
              <|> uVarOcc
              <|> uLabeledExprs
              <?> "contained expression"

uType :: Parser (UType S)
uType = expr

uString :: Lexer (UExpr S)
uString = do
  (s, pos) <- withPos $ strLit
  let addSrc = WithSrcH (Just pos)
  let cs = map (addSrc . charExpr) s
  return $ mkApp (addSrc "toList") $ addSrc $ UTabCon cs

uLit :: Parser (UExpr S)
uLit = withSrcH $ uLitParser
  where uLitParser = charExpr <$> charLit
                 <|> UIntLit  <$> intLit
                 <|> UFloatLit <$> doubleLit
                 <?> "literal"

charExpr :: Char -> UExpr' S
charExpr c = UPrimExpr $ ConExpr $ Lit $ Word8Lit $ fromIntegral $ fromEnum c

uVarOcc :: Parser (UExpr S)
uVarOcc = withSrcH $ try $ (UVar . (:>UnitH)) <$> (anyName <* notFollowedBy (sym ":"))

uHole :: Parser (UExpr S)
uHole = withSrcH $ underscore $> UHole

letAnnStr :: Parser LetAnn
letAnnStr =   (string "instance"   $> InstanceLet)
          <|> (string "superclass" $> SuperclassLet)
          <|> (string "noinline"   $> NoInlineLet)

topDecl :: Parser (UDecl S S)
topDecl = dataDef <|> topLet

topLet :: Parser (UDecl S S)
topLet = do
  lAnn <- (char '@' >> letAnnStr <* (eol <|> sc)) <|> return PlainLet
  ~(ULet _ (p, ann) rhs) <- decl
  let (ann', rhs') = addImplicitImplicitArgs ann rhs
  return $ ULet lAnn (p, ann') rhs'

-- Given a type signature, find all "implicit implicit args": lower-case
-- identifiers, not explicitly bound by Pi binders, not appearing on the left
-- hand side of an application. These identifiers are implicit in the sense
-- that they will be solved for by type inference, and also implicit in the
-- sense that the user did NOT explicitly annotate them as implicit.
findImplicitImplicitArgNames :: UType S -> [Name S]
findImplicitImplicitArgNames = undefined
-- findImplicitImplicitArgNames typ = filter (isLowerCaseName . getRawName) $ envNames $
--     freeUVars typ `envDiff` findVarsInAppLHS typ
--   where

--   isLowerCaseName :: RawName -> Bool
--   isLowerCaseName (RawName tag _) = isLower $ head $ tagToStr tag

--   -- Finds all variables used in the left hand of an application, which should
--   -- be filtered out and not automatically inferred.
--   findVarsInAppLHS :: UType S -> NameSet S
--   findVarsInAppLHS (WithSrcH _ typ') = case typ' of
--     -- base case
--     UApp _ (WithSrcH _ (UVar (v:>_))) x -> (v @> ()) <> findVarsInAppLHS x
--     -- recursive steps
--     UVar _ -> mempty
--     UPi (p, ann) _ ty ->
--       foldMap findVarsInAppLHS ann <> (findVarsInAppLHS ty `envDiff` boundUVars p)
--     UApp _ f x -> findVarsInAppLHS f <> findVarsInAppLHS x
--     ULam (p, ann) _ x ->
--       foldMap findVarsInAppLHS ann <> (findVarsInAppLHS x `envDiff` boundUVars p)
--     UDecl _ _ -> error "Unexpected let binding in type annotation"
--     -- UFor _ _ _ -> error "Unexpected for in type annotation"
--     -- UHole -> mempty
--     -- UTypeAnn v ty -> findVarsInAppLHS v <> findVarsInAppLHS ty
--     -- UTabCon _ -> error "Unexpected table constructor in type annotation"
--     -- UIndexRange low high ->
--     --   foldMap findVarsInAppLHS low <> foldMap findVarsInAppLHS high
--     -- UPrimExpr prim -> foldMap findVarsInAppLHS prim
--     -- UCase _ _ -> error "Unexpected case in type annotation"
--     -- URecord (Ext ulr _) -> foldMap findVarsInAppLHS ulr
--     -- UVariant _ _ val -> findVarsInAppLHS val
--     -- URecordTy (Ext ulr v) ->
--     --   foldMap findVarsInAppLHS ulr <> foldMap findVarsInAppLHS v
--     -- UVariantTy (Ext ulr v) ->
--     --   foldMap findVarsInAppLHS ulr <> foldMap findVarsInAppLHS v
--     -- UVariantLift _ val -> findVarsInAppLHS val
--     UIntLit  _ -> mempty
--     UFloatLit _ -> mempty

addImplicitImplicitArgs :: Maybe (UType S) -> UExpr S -> (Maybe (UType S), UExpr S)
addImplicitImplicitArgs Nothing e = (Nothing, e)
addImplicitImplicitArgs (Just typ) ex =
  let (ty', e') = foldr addImplicitArg (typ, ex) implicitVars
  in (Just ty', e')
  where
    implicitVars = findImplicitImplicitArgNames typ

    addImplicitArg :: Name S -> (UType S, UExpr S) -> (UType S, UExpr S)
    addImplicitArg v (ty, e) =
      ( ns $ UPi  (uPat, Nothing) ImplicitArrow ty
      , ns $ ULam (uPat, Nothing) ImplicitArrow e)
      where uPat = WithSrcH2 Nothing $ nameToPat v

superclassConstraints :: Parser [UType S]
superclassConstraints = optionalMonoid $ brackets $ uType `sepBy` sym ","

interfaceDef :: Parser (UDecl S S)
interfaceDef = undefined
-- interfaceDef = do
--   keyWord InterfaceKW
--   superclasses <- superclassConstraints
--   tyCon <- tyConDef
--   methods <- onePerLine $ do
--     v <- anyName
--     ty <- annot uType
--     return $ Bind $ v:>ty
--   return $ UInterface superclasses tyCon methods

dataDef :: Parser (UDecl S S)
dataDef = undefined
-- dataDef = do
--   keyWord DataKW
--   tyCon <- tyConDef
--   sym "="
--   dataCons <- onePerLine dataConDef
--   return $ UData tyCon dataCons

-- tyConDef :: Parser (UConDef S S)
-- tyConDef = do
--   con <- upperName <|> symName
--   bs <- manyNested $ label "type constructor parameter" $ do
--     v <- lowerName
--     ty <- annot containedExpr <|> return tyKind
--     return $ Bind $ v :> ty
--   return $ UConDef con bs
--   where tyKind = ns $ UPrimExpr $ TCExpr TypeKind

-- TODO: dependent types
-- dataConDef :: Parser UConDef
-- dataConDef = UConDef <$> upperName <*> manyNested dataConDefBinder

dataConDefBinder :: Parser (UAnnBinder S S)
dataConDefBinder = annBinder <|> (Ignore <$> containedExpr)

decl :: Parser (UDecl S S)
decl = do
  lhs <- simpleLet <|> funDefLet
  rhs <- sym "=" >> blockOrExpr
  return $ lhs rhs

instanceDef :: Bool -> Parser (UDecl S S)
instanceDef = undefined
-- instanceDef isNamed = do
--   name <- case isNamed of
--     False -> keyWord InstanceKW $> Nothing
--     True  -> keyWord NamedInstanceKW *> (Just . (:>()) <$> anyName) <* sym ":"
--   explicitArgs <- many defArg
--   constraints <- classConstraints
--   classTy <- uType
--   let implicitArgs = findImplicitImplicitArgNames $
--                        buildPiType explicitArgs Pure $
--                          foldr addClassConstraint classTy constraints
--   let argBinders =
--         [((ns (nameToPat v), Nothing), ImplicitArrow) | v <- implicitArgs] ++
--         explicitArgs                                                       ++
--         [((UnderscoreUPat, Just c)   , ClassArrow   ) | c <- constraints]
--   methods <- onePerLine instanceMethod
--   return $ UInstance name (toNest argBinders) classTy methods
--   where
--     addClassConstraint :: UType S -> UType S -> UType S
--     addClassConstraint c ty = ns $ UPi (UnderscoreUPat, Just c) ClassArrow ty

-- instanceMethod :: Parser UMethodDef
-- instanceMethod = do
--   v <- anyName
--   sym "="
--   rhs <- blockOrExpr
--   return $ UMethodDef (v:>()) rhs

simpleLet :: Parser (UExpr S -> UDecl S S)
simpleLet = label "let binding" $ do
  letAnn <- (InstanceLet <$ string "%instance" <* sc) <|> (pure PlainLet)
  p <- try $ (letPat <|> leafPat) <* lookAhead (sym "=" <|> sym ":")
  typeAnn <- optional $ annot uType
  return $ ULet letAnn (p, typeAnn)

letPat :: Parser (UPat S S)
letPat = withSrcH2 $ nameToPat <$> anyName

funDefLet :: Parser (UExpr S -> UDecl S S)
funDefLet = label "function definition" $ mayBreak $ do
  keyWord DefKW
  v <- letPat
  cs <- classConstraints
  argBinders <- many defArg
  (eff, ty) <- label "result type annotation" $ annot effectiveType
  when (null argBinders && eff /= Pure) $ fail "Nullary def can't have effects"
  let bs = map classAsBinder cs ++ argBinders
  let funTy = buildPiType bs eff ty
  let letBinder = (v, Just funTy)
  let lamBinders = flip map bs \(UPatAnnArrow (p,_) arr) -> ((p,Nothing), arr)
  return \body -> ULet PlainLet letBinder (buildLam lamBinders body)
  where
    classAsBinder :: UType S -> UPatAnnArrow S S
    classAsBinder ty = UPatAnnArrow (UnderscoreUPat, Just ty) ClassArrow

defArg :: Parser (UPatAnnArrow S S)
defArg = label "def arg" $ do
  (p, ty) <-parens ((,) <$> pat <*> annot uType)
  arr <- arrow (return ()) <|> return (PlainArrow ())
  return (UPatAnnArrow (p, Just ty) arr)

classConstraints :: Parser [UType S]
classConstraints = label "class constraints" $
  optionalMonoid $ brackets $ mayNotPair $ uType `sepBy` sym ","

buildPiType :: [UPatAnnArrow S S] -> EffectRow S -> UType S -> UType S
buildPiType [] Pure ty = ty
buildPiType [] _ _ = error "shouldn't be possible"
buildPiType (UPatAnnArrow (p, patTy) arr : bs) eff resTy = ns case bs of
  [] -> UPi (p, patTy) (fmap (const eff ) arr) resTy
  _  -> UPi (p, patTy) (fmap (const Pure) arr) $ buildPiType bs eff resTy

effectiveType :: Parser (EffectRow S, UType S)
effectiveType = (,) <$> effects <*> uType

effects :: Parser (EffectRow S)
effects = braces someEffects <|> return Pure
  where
    someEffects = do
      effs <- effect `sepBy` sym ","
      v <- optional $ symbol "|" >> lowerName
      return $ EffectRow (S.fromList effs) v

effect :: Parser (Effect S)
effect =   (RWSEffect <$> rwsName <*> anyCaseName)
       <|> (keyWord ExceptKW $> ExceptionEffect)
       <|> (keyWord IOKW     $> IOEffect)
       <?> "effect (Accum h | Read h | State h | Except | IO)"

rwsName :: Parser RWS
rwsName =   (keyWord WriteKW $> Writer)
        <|> (keyWord ReadKW  $> Reader)
        <|> (keyWord StateKW $> State)

uLamExpr :: Parser (UExpr S)
uLamExpr = do
  sym "\\"
  bs <- some patAnn
  arrowType <-
    (argTerm >> return (PlainArrow ()))
    <|> (arrow (return ()) >>= \case
          PlainArrow _ -> fail
            "To construct an explicit lambda function, use '.' instead of '->'\n"
          TabArrow -> fail
            "To construct a table, use 'for i. body' instead of '\\i => body'\n"
          arr -> return arr)
  body <- blockOrExpr
  return $ buildLam (map (,arrowType) bs) body

buildLam :: [(UPatAnn S S, UArrow)] -> UExpr S -> UExpr S
buildLam binders body@(WithSrcH pos _) = case binders of
  [] -> body
  -- TODO: join with source position of binders too
  (b,arr):bs -> WithSrcH (joinPos pos' pos) $ ULam b arr $ buildLam bs body
     where (WithSrcH2 pos' _, _) = b

buildFor :: SrcPos -> Direction -> [UPatAnn S S] -> UExpr S -> UExpr S
buildFor pos dir binders body = case binders of
  [] -> body
  b:bs -> WithSrcH (Just pos) $ UFor dir b $ buildFor pos dir bs body

uViewExpr :: Parser (UExpr S)
uViewExpr = do
  keyWord ViewKW
  bs <- some patAnn
  argTerm
  body <- blockOrExpr
  return $ buildLam (zip bs (repeat TabArrow)) body

uForExpr :: Parser (UExpr S)
uForExpr = do
  ((dir, trailingUnit), pos) <- withPos $
          (keyWord ForKW  $> (Fwd, False))
      <|> (keyWord For_KW $> (Fwd, True ))
      <|> (keyWord RofKW  $> (Rev, False))
      <|> (keyWord Rof_KW $> (Rev, True ))
  e <- buildFor pos dir <$> (some patAnn <* argTerm) <*> blockOrExpr
  if trailingUnit
    then return $ ns $ UDecl (ULet PlainLet (UnderscoreUPat, Nothing) e) $
                                ns unitExpr
    else return e

nameToPat :: Name S -> UPat' S S
nameToPat v = UPatBinder (SourceBinder v UnitH)

unitExpr :: UExpr' S
unitExpr = UPrimExpr $ ConExpr UnitCon

ns :: e n -> WithSrcH e n
ns = WithSrcH Nothing

blockOrExpr :: Parser (UExpr S)
blockOrExpr =  block <|> expr

unitCon :: Parser (UExpr S)
unitCon = withSrcH $ symbol "()" $> unitExpr

uTabCon :: Parser (UExpr S)
uTabCon = withSrcH $ do
  xs <- brackets $ expr `sepBy` sym ","
  return $ UTabCon xs

type UStatement = (Either (UDecl S S) (UExpr S), SrcPos)

block :: Parser (UExpr S)
block = withIndent $ do
  statements <- mayNotBreak $ uStatement `sepBy1` (semicolon <|> try nextLine)
  case last statements of
    (Left _, _) -> fail "Last statement in a block must be an expression."
    _           -> return $ wrapUStatements statements

wrapUStatements :: [UStatement] -> UExpr S
wrapUStatements statements = case statements of
  [(Right e, _)] -> e
  (s, pos):rest -> WithSrcH (Just pos) $ case s of
    Left  d -> UDecl d $ wrapUStatements rest
    Right e -> UDecl d $ wrapUStatements rest
      where d = ULet PlainLet (UnderscoreUPat, Nothing) e
  [] -> error "Shouldn't be reachable"

uStatement :: Parser UStatement
uStatement = withPos $   liftM Left  (instanceDef True <|> decl)
                     <|> liftM Right expr

-- TODO: put the `try` only around the `x:` not the annotation itself
uPiType :: Parser (UExpr S)
uPiType = withSrcH $ UPi <$> piBinderPat <*> arrow effects <*> uType
  where piBinderPat = do
          b <- annBinder
          return $ case b of
            SourceBinder n a@(WithSrcH pos _) ->
              (WithSrcH2 pos $ nameToPat n, Just a)
            Ignore a -> (UnderscoreUPat, Just a)

annBinder :: Parser (UAnnBinder S S)
annBinder = try $ namedBinder <|> anonBinder

namedBinder :: Parser (UAnnBinder S S)
namedBinder = label "named annoted binder" $ do
  v <- lowerName
  ty <- annot containedExpr
  return $ SourceBinder v ty

anonBinder :: Parser (UAnnBinder S S)
anonBinder =
  label "anonymous annoted binder" $ Ignore <$> (underscore >> sym ":"
                                                            >> containedExpr)

arrow :: Parser eff -> Parser (ArrowP eff)
arrow p =   (sym "->"  >> liftM PlainArrow p)
        <|> (sym "=>"  $> TabArrow)
        <|> (sym "--o" $> LinArrow)
        <|> (sym "?->" $> ImplicitArrow)
        <|> (sym "?=>" $> ClassArrow)
        <?> "arrow"

caseExpr :: Parser (UExpr S)
caseExpr = withSrcH $ do
  keyWord CaseKW
  e <- expr
  keyWord OfKW
  alts <- onePerLine $ UAlt <$> pat <*> (sym "->" *> blockOrExpr)
  return $ UCase e alts

ifExpr :: Parser (UExpr S)
ifExpr = undefined
-- ifExpr = withSrc $ do
--   keyWord IfKW
--   e <- expr
--   (alt1, maybeAlt2) <- oneLineThenElse <|> blockThenElse
--   let alt2 = case maybeAlt2 of
--         Nothing  -> ns unitExpr
--         Just alt -> alt
--   return $ UCase e
--       [ UAlt (globalEnumPat "True" ) alt1
--       , UAlt (globalEnumPat "False") alt2]

oneLineThenElse :: Parser (UExpr S, Maybe (UExpr S))
oneLineThenElse = do
  keyWord ThenKW
  alt1 <- eitherP block expr
  case alt1 of
    Left  e -> return (e, Nothing)
    Right e -> do
      alt2 <- optional $ keyWord ElseKW >> blockOrExpr
      return (e, alt2)

blockThenElse :: Parser (UExpr S, Maybe (UExpr S))
blockThenElse = withIndent $ mayNotBreak $ do
  alt1 <- keyWord ThenKW >> blockOrExpr
  alt2 <- optional $ do
    try $ nextLine >> keyWord ElseKW
    blockOrExpr
  return (alt1, alt2)

-- globalEnumPat :: Tag -> UPat S S
-- globalEnumPat s = ns $ UPatCon (GlobalName s) Empty

onePerLine :: Parser a -> Parser [a]
onePerLine p =   liftM (:[]) p
             <|> (withIndent $ mayNotBreak $ p `sepBy1` try nextLine)

pat :: Parser (UPat S S)
pat = mayNotPair $ makeExprParser leafPat patOps

leafPat :: Parser (UPat S S)
leafPat = undefined
-- leafPat =
--       (withSrc (symbol "()" $> UPatUnit))
--   <|> parens (mayPair $ makeExprParser leafPat patOps)
--   <|> (withSrc $
--           (UPatBinder <$>  (   (Bind <$> (:>()) <$> lowerName)
--                            <|> (underscore $> Ignore ())))
--       <|> (UPatCon    <$> upperName <*> manyNested pat)
--       <|> (variantPat `fallBackTo` recordPat)
--       <|> brackets (UPatTable <$> leafPat `sepBy` sym ",")
--   )
--   where pun pos l = WithSrc (Just pos) $ nameToPat $ mkName l
--         def pos = WithSrc (Just pos) $ UPatBinder (Ignore ())
--         variantPat = parseVariant leafPat UPatVariant UPatVariantLift
--         recordPat = UPatRecord <$> parseLabeledItems "," "=" leafPat
--                                                      (Just pun) (Just def)

-- TODO: add user-defined patterns
patOps :: [[Operator Parser (UPat S S)]]
patOps = [[InfixR patPairOp]]

patPairOp :: Parser (UPat S S -> UPat S S -> UPat S S)
patPairOp = undefined
-- patPairOp = do
--   allowed <- asks canPair
--   if allowed
--     then sym "," $> \x y -> joinSrc x y $ UPatPair x y
--     else fail "pair pattern not allowed outside parentheses"

annot :: Parser a -> Parser a
annot p = label "type annotation" $ sym ":" >> p

patAnn :: Parser (UPatAnn S S)
patAnn = label "pattern" $ (,) <$> pat <*> optional (annot containedExpr)

uPrim :: Parser (UExpr S)
uPrim = withSrcH $ do
  s <- primName
  case s of
    "ffi" -> do
      f <- lexeme $ some nameTailChar
      retTy <- leafExpr
      args <- some leafExpr
      return $ UPrimExpr $ OpExpr $ FFICall f retTy args
    _ -> case strToPrimName s of
      Just prim -> UPrimExpr <$> traverse (const leafExpr) prim
      Nothing -> fail $ "Unrecognized primitive: " ++ s

uVariantExpr :: Parser (UExpr S)
uVariantExpr = undefined
-- uVariantExpr = withSrc $ parseVariant expr UVariant UVariantLift

parseVariant :: Parser a -> (LabeledItems () -> Label -> a -> b) -> (LabeledItems () -> a -> b) -> Parser b
parseVariant = undefined
-- parseVariant subparser buildLabeled buildExt =
--   bracketed (symbol "{|") (symbol "|}") $ do
--     let parseInactive = try $ fieldLabel <* notFollowedBy (symbol "=")
--     inactiveLabels <- parseInactive `endBy1` (symbol "|") <|> pure []
--     let inactiveItems = foldr (<>) NoLabeledItems $ map (flip labeledSingleton ()) inactiveLabels
--     let parseLabeled = do l <- fieldLabel
--                           symbol "="
--                           buildLabeled inactiveItems l <$> subparser
--     let parseExt = do symbol "..."
--                       buildExt inactiveItems <$> subparser
--     parseLabeled <|> parseExt

uLabeledExprs :: Parser (UExpr S)
uLabeledExprs = undefined
-- uLabeledExprs = withSrc $
--     (URecord <$> build "," "=" (Just varPun) Nothing)
--     `fallBackTo` (URecordTy <$> build "&" ":" Nothing Nothing)
--     `fallBackTo` (UVariantTy <$> build "|" ":" Nothing Nothing)
--   where build sep bindwith = parseLabeledItems sep bindwith expr

varPun :: SrcPos -> Label -> UExpr S
varPun pos str = WithSrcH (Just pos) $ UVar (mkName str :> UnitH)

uDoSugar :: Parser (UExpr S)
uDoSugar = withSrcH $ do
  keyWord DoKW
  body <- blockOrExpr
  return $ ULam (WithSrcH2 Nothing UPatUnit, Nothing) (PlainArrow ()) body

uIsoSugar :: Parser (UExpr S)
uIsoSugar = undefined
-- uIsoSugar = withSrc (char '#' *> options) where
--   options = (recordFieldIso <$> fieldLabel)
--             <|> char '?' *> (variantFieldIso <$> fieldLabel)
--             <|> char '&' *> (recordZipIso <$> fieldLabel)
--             <|> char '|' *> (variantZipIso <$> fieldLabel)
--   var s = ns $ UVar $ mkName s :> ()
--   patb s = ns $ nameToPat $ mkName s
--   plain = PlainArrow ()
--   lam p b = ns $ ULam (p, Nothing) plain b
--   recordFieldIso field =
--     UApp plain (var "MkIso") $
--       ns $ URecord $ NoExt $
--         labeledSingleton "fwd" (lam
--           (ns $ UPatRecord $ Ext (labeledSingleton field (patb "x"))
--                                        (Just $ patb "r"))
--           $ (var "(,)") `mkApp` (var "x") `mkApp` (var "r")
--         )
--         <> labeledSingleton "bwd" (lam
--           (ns $ UPatPair (patb "x") (patb "r"))
--           $ ns $ URecord $ Ext (labeledSingleton field $ var "x")
--                                $ Just $ var "r"
--         )
--   variantFieldIso field =
--     UApp plain (var "MkIso") $
--       ns $ URecord $ NoExt $
--         labeledSingleton "fwd" (lam (patb "v") $ ns $ UCase (var "v")
--             [ UAlt (ns $ UPatVariant NoLabeledItems field (patb "x"))
--                 $ var "Left" `mkApp` var "x"
--             , UAlt (ns $ UPatVariantLift (labeledSingleton field ()) (patb "r"))
--                 $ var "Right" `mkApp` var "r"
--             ]
--         )
--         <> labeledSingleton "bwd" (lam (patb "v") $ ns $ UCase (var "v")
--             [ UAlt (ns $ UPatCon (mkName "Left") (toNest [patb "x"]))
--                 $ ns $ UVariant NoLabeledItems field $ var "x"
--             , UAlt (ns $ UPatCon (mkName "Right") (toNest [patb "r"]))
--                 $ ns $ UVariantLift (labeledSingleton field ()) $ var "r"
--             ]
--         )
--   recordZipIso field =
--     UApp plain (var "MkIso") $
--       ns $ URecord $ NoExt $
--         labeledSingleton "fwd" (lam
--           (ns $ UPatPair
--             (ns $ UPatRecord $ Ext NoLabeledItems $ Just $ patb "l")
--             (ns $ UPatRecord $ Ext (labeledSingleton field $ patb "x")
--                                    $ Just $ patb "r"))
--           $ (var "(,)")
--             `mkApp` (ns $ URecord $ Ext (labeledSingleton field $ var "x")
--                                         $ Just $ var "l")
--             `mkApp` (ns $ URecord $ Ext NoLabeledItems $ Just $ var "r")
--         )
--         <> labeledSingleton "bwd" (lam
--           (ns $ UPatPair
--             (ns $ UPatRecord $ Ext (labeledSingleton field $ patb "x")
--                                    $ Just $ patb "l")
--             (ns $ UPatRecord $ Ext NoLabeledItems $ Just $ patb "r"))
--           $ (var "(,)")
--             `mkApp` (ns $ URecord $ Ext NoLabeledItems $ Just $ var "l")
--             `mkApp` (ns $ URecord $ Ext (labeledSingleton field $ var "x")
--                                         $ Just $ var "r")
--         )
--   variantZipIso field =
--     UApp plain (var "MkIso") $
--       ns $ URecord $ NoExt $
--         labeledSingleton "fwd" (lam (patb "v") $ ns $ UCase (var "v")
--             [ UAlt (ns $ UPatCon (mkName "Left") (toNest [patb "l"]))
--                 $ var "Left" `mkApp` (ns $
--                     UVariantLift (labeledSingleton field ()) $ var "l")
--             , UAlt (ns $ UPatCon (mkName "Right") (toNest [patb "w"]))
--                 $ ns $ UCase (var "w")
--                 [ UAlt (ns $ UPatVariant NoLabeledItems field (patb "x"))
--                     $ var "Left" `mkApp` (ns $
--                         UVariant NoLabeledItems field $ var "x")
--                 , UAlt (ns $ UPatVariantLift (labeledSingleton field ())
--                                              (patb "r"))
--                     $ var "Right" `mkApp` var "r"
--                 ]
--             ]
--         )
--         <> labeledSingleton "bwd" (lam (patb "v") $ ns $ UCase (var "v")
--             [ UAlt (ns $ UPatCon (mkName "Left") (toNest [patb "w"]))
--                 $ ns $ UCase (var "w")
--                 [ UAlt (ns $ UPatVariant NoLabeledItems field (patb "x"))
--                     $ var "Right" `mkApp` (ns $
--                         UVariant NoLabeledItems field $ var "x")
--                 , UAlt (ns $ UPatVariantLift (labeledSingleton field ())
--                                              (patb "r"))
--                     $ var "Left" `mkApp` var "r"
--                 ]
--             , UAlt (ns $ UPatCon (mkName "Right") (toNest [patb "l"]))
--                 $ var "Right" `mkApp` (ns $
--                     UVariantLift (labeledSingleton field ()) $ var "l")
--             ]
--         )

parseLabeledItems
  :: String -> String -> Parser a
  -> Maybe (SrcPos -> Label -> a) -> Maybe (SrcPos -> a)
  -> Parser (ExtLabeledItems a a)
parseLabeledItems = undefined
-- parseLabeledItems sep bindwith itemparser punner tailDefault =
--   bracketed lBrace rBrace $ atBeginning
--   where
--     atBeginning = someItems
--                   <|> (symbol sep >> (stopAndExtend <|> stopWithoutExtend))
--                   <|> stopWithoutExtend
--     stopWithoutExtend = return $ NoExt NoLabeledItems
--     stopAndExtend = do
--       ((), pos) <- withPos $ symbol "..."
--       rest <- case tailDefault of
--         Just def -> itemparser <|> pure (def pos)
--         Nothing -> itemparser
--       return $ Ext NoLabeledItems (Just rest)
--     beforeSep = (symbol sep >> afterSep) <|> stopWithoutExtend
--     afterSep = someItems <|> stopAndExtend <|> stopWithoutExtend
--     someItems = do
--       (l, pos) <- withPos fieldLabel
--       let explicitBound = symbol bindwith *> itemparser
--       itemVal <- case punner of
--         Just punFn -> explicitBound <|> pure (punFn pos l)
--         Nothing -> explicitBound
--       rest <- beforeSep
--       return $ prefixExtLabeledItems (labeledSingleton l itemVal) rest

-- Combine two parsers such that if the first fails, we try the second one.
-- If both fail, consume the same amount of input as the parser that
-- consumed the most input, and use its error message. Useful if you want
-- to parse multiple things, but they share some common starting symbol, and
-- you don't want the first one failing to prevent the second from succeeding.
-- Unlike using `try` and/or (<|>), if both parsers fail and either one consumes
-- input, parsing is aborted. Also, if both parsers consume the same amount of
-- input, we combine the symbols each was expecting.
fallBackTo :: Parser a -> Parser a -> Parser a
fallBackTo optionA optionB = do
  startState <- getParserState
  resA <- observing $ optionA
  case resA of
    Right val -> return val
    Left errA -> do
      stateA <- getParserState
      updateParserState $ const startState
      resB <- observing $ optionB
      case resB of
        Right val -> return val
        Left errB -> case compare (errorOffset errA) (errorOffset errB) of
          LT -> parseError errB
          GT -> updateParserState (const stateA) >> parseError errA
          EQ -> case (errA, errB) of
            -- Combine what each was expecting.
            (TrivialError offset unexpA expA, TrivialError _ unexpB expB)
                -> parseError $ TrivialError offset (unexpA <|> unexpB)
                                                    (expA <> expB)
            _ -> fail $ "Multiple failed parse attempts:\n"
                  <> parseErrorPretty errA <> "\n" <> parseErrorPretty errB

-- === infix ops ===

-- literal symbols here must only use chars from `symChars`
ops :: [[Operator Parser (UExpr S)]]
ops =
  [ [InfixL $ sym "." $> mkGenApp TabArrow, symOp "!"]
  , [InfixL $ sc $> mkApp]
  , [prefixNegOp]
  , [anySymOp] -- other ops with default fixity
  , [symOp "+", symOp "-", symOp "||", symOp "&&",
     InfixR $ sym "=>" $> mkArrow TabArrow,
     InfixL $ opWithSrc $ backquoteName >>= (return . binApp),
     symOp "<<<", symOp ">>>", symOp "<<&", symOp "&>>"]
  , [annotatedExpr]
  , [InfixR $ mayBreak (infixSym "$") $> mkApp]
  , [symOp "+=", symOp ":=", InfixL $ pairingSymOpP "|", InfixR infixArrow]
  , [InfixR $ pairingSymOpP "&", InfixR $ pairingSymOpP ","]
  , indexRangeOps
  ]

opWithSrc :: Parser (SrcPos -> a -> a -> a)
          -> Parser (a -> a -> a)
opWithSrc p = do
  (f, pos) <- withPos p
  return $ f pos

anySymOp :: Operator Parser (UExpr S)
anySymOp = InfixL $ opWithSrc $ do
  s <- label "infix operator" (mayBreak anySym)
  return $ binApp $ mkSymName s

infixSym :: String -> Parser ()
infixSym s = mayBreak $ sym s

symOp :: String -> Operator Parser (UExpr S)
symOp s = InfixL $ symOpP s

symOpP :: String -> Parser (UExpr S -> UExpr S -> UExpr S)
symOpP s = opWithSrc $ do
  label "infix operator" (infixSym s)
  return $ binApp $ mkSymName s

pairingSymOpP :: String -> Parser (UExpr S -> UExpr S -> UExpr S)
pairingSymOpP s = opWithSrc $ do
  allowed <- asks canPair
  if allowed
    then infixSym s >> return (binApp (mkSymName s))
    else fail $ "Unexpected delimiter " <> s

mkSymName :: String -> Name S
mkSymName s = mkName $ "(" <> s <> ")"

prefixNegOp :: Operator Parser (UExpr S)
prefixNegOp = Prefix $ label "negation" $ do
  ((), pos) <- withPos $ sym "-"
  let f = WithSrcH (Just pos) "neg"
  return \case
    -- Special case: negate literals directly
    WithSrcH litpos (IntLitExpr i)
      -> WithSrcH (joinPos (Just pos) litpos) (IntLitExpr (-i))
    WithSrcH litpos (FloatLitExpr i)
      -> WithSrcH (joinPos (Just pos) litpos) (FloatLitExpr (-i))
    x -> mkApp f x

binApp :: Name S -> SrcPos -> UExpr S -> UExpr S -> UExpr S
binApp f pos x y = (f' `mkApp` x) `mkApp` y
  where f' = WithSrcH (Just pos) $ UVar (f:>UnitH)

mkGenApp :: UArrow -> UExpr S -> UExpr S -> UExpr S
mkGenApp arr f x = joinSrcH f x $ UApp arr f x

mkApp :: UExpr S -> UExpr S -> UExpr S
mkApp f x = joinSrcH f x $ UApp (PlainArrow ()) f x

infixArrow :: Parser (UType S -> UType S -> UType S)
infixArrow = do
  notFollowedBy (sym "=>")  -- table arrows have special fixity
  (arr, pos) <- withPos $ arrow effects
  return \a b -> WithSrcH (Just pos) $ UPi (UnderscoreUPat, Just a) arr b

mkArrow :: Arrow S -> UExpr S -> UExpr S -> UExpr S
mkArrow arr a b = joinSrcH a b $ UPi (UnderscoreUPat, Just a) arr b

withSrc :: Parser a -> Parser (WithSrc a)
withSrc p = do
  (x, pos) <- withPos p
  return $ WithSrc (Just pos) x

withSrcH :: Parser (e n) -> Parser (WithSrcH e n)
withSrcH p = do
  (x, pos) <- withPos p
  return $ WithSrcH (Just pos) x

withSrcH2 :: Parser (b n l) -> Parser (WithSrcH2 b n l)
withSrcH2 p = do
  (x, pos) <- withPos p
  return $ WithSrcH2 (Just pos) x

joinSrcH :: WithSrcH a n -> WithSrcH b n -> c n -> WithSrcH c n
joinSrcH (WithSrcH p1 _) (WithSrcH p2 _) x = WithSrcH (joinPos p1 p2) x

joinSrc :: WithSrc a -> WithSrc b -> c -> WithSrc c
joinSrc (WithSrc p1 _) (WithSrc p2 _) x = WithSrc (joinPos p1 p2) x

joinPos :: Maybe SrcPos -> Maybe SrcPos -> Maybe SrcPos
joinPos Nothing p = p
joinPos p Nothing = p
joinPos (Just (l, h)) (Just (l', h')) = Just (min l l', max h h')

indexRangeOps :: [Operator Parser (UExpr S)]
indexRangeOps =
  [ Prefix    $ symPos ".."   <&> \pos h   -> range pos  Unlimited       (InclusiveLim h)
  , inpostfix $ symPos ".."   <&> \pos l h -> range pos (InclusiveLim l) (limFromMaybe h)
  , inpostfix $ symPos "<.."  <&> \pos l h -> range pos (ExclusiveLim l) (limFromMaybe h)
  , Prefix    $ symPos "..<"  <&> \pos h   -> range pos  Unlimited       (ExclusiveLim h)
  , InfixL    $ symPos "..<"  <&> \pos l h -> range pos (InclusiveLim l) (ExclusiveLim h)
  , InfixL    $ symPos "<..<" <&> \pos l h -> range pos (ExclusiveLim l) (ExclusiveLim h) ]
  where
    range pos l h = WithSrcH (Just pos) $ UIndexRange l h
    symPos s = snd <$> withPos (sym s)

limFromMaybe :: Maybe a -> Limit a
limFromMaybe Nothing = Unlimited
limFromMaybe (Just x) = InclusiveLim x

annotatedExpr :: Operator Parser (UExpr S)
annotatedExpr = undefined
-- annotatedExpr = InfixL $ opWithSrc $
--   sym ":" $> (\pos v ty -> WithSrc (Just pos) $ UTypeAnn v ty)

inpostfix :: Parser (UExpr S -> Maybe (UExpr S) -> UExpr S) -> Operator Parser (UExpr S)
inpostfix = inpostfix' expr

inpostfix' :: Parser a -> Parser (a -> Maybe a -> a) -> Operator Parser a
inpostfix' p op = Postfix $ do
  f <- op
  rest <- optional p
  return \x -> f x rest

mkName :: String -> Name S
mkName s = undefined

-- === lexemes ===

-- These `Lexer` actions must be non-overlapping and never consume input on failure
type Lexer = Parser

data KeyWord = DefKW | ForKW | For_KW | RofKW | Rof_KW | CaseKW | OfKW
             | ReadKW | WriteKW | StateKW | DataKW | InterfaceKW
             | InstanceKW | WhereKW | IfKW | ThenKW | ElseKW | DoKW
             | ExceptKW | IOKW | ViewKW | ImportKW | NamedInstanceKW

upperName :: Lexer (Name S)
upperName = liftM mkName $ label "upper-case name" $ lexeme $
  checkNotKeyword $ (:) <$> upperChar <*> many nameTailChar

lowerName  :: Lexer (Name S)
lowerName = liftM mkName $ label "lower-case name" $ lexeme $
  checkNotKeyword $ (:) <$> lowerChar <*> many nameTailChar

anyCaseName  :: Lexer (Name S)
anyCaseName = lowerName <|> upperName

anyName  :: Lexer (Name S)
anyName = lowerName <|> upperName <|> symName

checkNotKeyword :: Parser String -> Parser String
checkNotKeyword p = try $ do
  s <- p
  failIf (s `elem` keyWordStrs) $ show s ++ " is a reserved word"
  return s

keyWord :: KeyWord -> Lexer ()
keyWord kw = lexeme $ try $ string s >> notFollowedBy nameTailChar
  where
    s = case kw of
      DefKW  -> "def"
      ForKW  -> "for"
      RofKW  -> "rof"
      For_KW  -> "for_"
      Rof_KW  -> "rof_"
      CaseKW -> "case"
      IfKW   -> "if"
      ThenKW -> "then"
      ElseKW -> "else"
      OfKW   -> "of"
      ReadKW  -> "Read"
      WriteKW -> "Accum"
      StateKW -> "State"
      ExceptKW -> "Except"
      IOKW     -> "IO"
      DataKW -> "data"
      InterfaceKW -> "interface"
      InstanceKW -> "instance"
      NamedInstanceKW -> "named-instance"
      WhereKW -> "where"
      DoKW   -> "do"
      ViewKW -> "view"
      ImportKW -> "import"

keyWordStrs :: [String]
keyWordStrs = ["def", "for", "for_", "rof", "rof_", "case", "of", "llam",
               "Read", "Write", "Accum", "Except", "IO", "data", "interface",
               "instance", "named-instance", "where", "if", "then", "else", "do", "view", "import"]

fieldLabel :: Lexer Label
fieldLabel = label "field label" $ lexeme $
  checkNotKeyword $ (:) <$> (lowerChar <|> upperChar) <*> many nameTailChar

primName :: Lexer String
primName = lexeme $ try $ char '%' >> some alphaNumChar

charLit :: Lexer Char
charLit = lexeme $ char '\'' >> L.charLiteral <* char '\''

strLit :: Lexer String
strLit = lexeme $ char '"' >> manyTill L.charLiteral (char '"')

intLit :: Lexer Int
intLit = lexeme $ try $ L.decimal <* notFollowedBy (char '.')

doubleLit :: Lexer Double
doubleLit = lexeme $
      try L.float
  <|> try (fromIntegral <$> (L.decimal :: Parser Int) <* char '.')

knownSymStrs :: [String]
knownSymStrs = [".", ":", "!", "=", "-", "+", "||", "&&", "$", "&", "|", ",", "+=", ":=",
                "->", "=>", "?->", "?=>", "--o", "--", "<<<", ">>>", "<<&", "&>>",
                "..", "<..", "..<", "..<", "<..<"]

-- string must be in `knownSymStrs`
sym :: String -> Lexer ()
sym s = lexeme $ try $ string s >> notFollowedBy symChar

anySym :: Lexer String
anySym = lexeme $ try $ do
  s <- some symChar
  -- TODO: consider something faster than linear search here
  failIf (s `elem` knownSymStrs) ""
  return s

symName :: Lexer (Name S)
symName = label "symbol name" $ lexeme $ try $ do
  s <- between (char '(') (char ')') $ some symChar
  return $ mkName $ "(" <> s <> ")"

backquoteName :: Lexer (Name S)
backquoteName = label "backquoted name" $
  lexeme $ try $ between (char '`') (char '`') (upperName <|> lowerName)

-- brackets and punctuation
-- (can't treat as sym because e.g. `((` is two separate lexemes)
lParen, rParen, lBracket, rBracket, lBrace, rBrace, semicolon, underscore :: Lexer ()

lParen    = notFollowedBy symName >> notFollowedBy unitCon >> charLexeme '('
rParen    = charLexeme ')'
lBracket  = charLexeme '['
rBracket  = charLexeme ']'
lBrace    = charLexeme '{'
rBrace    = charLexeme '}'
semicolon = charLexeme ';'
underscore = charLexeme '_'

charLexeme :: Char -> Parser ()
charLexeme c = void $ lexeme $ char c

nameTailChar :: Parser Char
nameTailChar = alphaNumChar <|> char '\'' <|> char '_'

symChar :: Parser Char
symChar = choice $ map char symChars

symChars :: [Char]
symChars = ".,!$^&*:-~+/=<>|?\\@"

-- === Util ===

runTheParser :: String -> Parser a -> Either (ParseErrorBundle String Void) a
runTheParser s p = parse (runReaderT p (ParseCtx 0 False False)) "" s

sc :: Parser ()
sc = L.space space lineComment empty

lineComment :: Parser ()
lineComment = do
  try $ string "--" >> notFollowedBy (void (char 'o'))
  void (takeWhileP (Just "char") (/= '\n'))

emptyLines :: Parser ()
emptyLines = void $ many (sc >> eol)

outputLines :: Parser ()
outputLines = void $ many (symbol ">" >> takeWhileP Nothing (/= '\n') >> ((eol >> return ()) <|> eof))

stringLiteral :: Parser String
stringLiteral = char '"' >> manyTill L.charLiteral (char '"') <* sc

space :: Parser ()
space = do
  consumeNewLines <- asks canBreak
  if consumeNewLines
    then space1
    else void $ takeWhile1P (Just "white space") (`elem` (" \t" :: String))

mayBreak :: Parser a -> Parser a
mayBreak p = local (\ctx -> ctx { canBreak = True }) p

mayNotBreak :: Parser a -> Parser a
mayNotBreak p = local (\ctx -> ctx { canBreak = False }) p

mayPair :: Parser a -> Parser a
mayPair p = local (\ctx -> ctx { canPair = True }) p

mayNotPair :: Parser a -> Parser a
mayNotPair p = local (\ctx -> ctx { canPair = False }) p

optionalMonoid :: Monoid a => Parser a -> Parser a
optionalMonoid p = p <|> return mempty

nameString :: Parser String
nameString = lexeme . try $ (:) <$> lowerChar <*> many alphaNumChar

thisNameString :: String -> Parser ()
thisNameString s = lexeme $ try $ string s >> notFollowedBy alphaNumChar

uint :: Parser Int
uint = L.decimal <* sc

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser ()
symbol s = void $ L.symbol sc s

argTerm :: Parser ()
argTerm = mayNotBreak $ sym "."

bracketed :: Parser () -> Parser () -> Parser a -> Parser a
bracketed left right p = between left right $ mayBreak $ sc >> p

parens :: Parser a -> Parser a
parens p = bracketed lParen rParen p

brackets :: Parser a -> Parser a
brackets p = bracketed lBracket rBracket p

braces :: Parser a -> Parser a
braces p = bracketed lBrace rBrace p

-- manyNested :: Parser a -> Parser (Nest a)
-- manyNested p = toNest <$> many p

withPos :: Parser a -> Parser (a, SrcPos)
withPos p = do
  n <- getOffset
  x <- p
  n' <- getOffset
  return $ (x, (n, n'))

nextLine :: Parser ()
nextLine = do
  eol
  n <- asks curIndent
  void $ mayNotBreak $ many $ try (sc >> eol)
  void $ replicateM n (char ' ')

withSource :: Parser a -> Parser (String, a)
withSource p = do
  s <- getInput
  (x, (start, end)) <- withPos p
  return (take (end - start) s, x)

withIndent :: Parser a -> Parser a
withIndent p = do
  nextLine
  indent <- liftM length $ some (char ' ')
  local (\ctx -> ctx { curIndent = curIndent ctx + indent }) $ p

eol :: Parser ()
eol = void MC.eol

eolf :: Parser ()
eolf = eol <|> eof

failIf :: Bool -> String -> Parser ()
failIf True s = fail s
failIf False _ = return ()

_debug :: Show a => String -> Parser a -> Parser a
_debug s m = mapReaderT (Text.Megaparsec.Debug.dbg s) m
