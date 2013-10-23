{-# LANGUAGE TupleSections, TypeFamilies, ScopedTypeVariables, FlexibleContexts #-}
module Parenthesize where

import Prelude hiding (mapM)

import Control.Applicative        ((<$>), (<*>))
import Control.Monad.Trans.Writer (Writer, runWriter, execWriter, tell)
import Data.Either                (partitionEithers)
import Data.Generics              (Data, everywhereM, extM, gmapM, extT)
import Data.List                  (group, sort)
import Data.Maybe                 (fromMaybe, catMaybes)
import Data.Monoid                (Monoid(..))
import Data.Traversable           (mapM)
import Language.Haskell.Exts.Annotated
import Language.Haskell.Exts.Annotated.Simplify
import qualified Language.Haskell.Exts              as HSE
import qualified Language.Haskell.TH                as TH
import qualified Language.Haskell.TH.Lift           as TH
import qualified Language.Haskell.TH.Quote          as TH
import qualified Data.Map                           as Map
import qualified Data.Generics.SYB.WithClass.Basics as SWC

import HasFix.Adjust (adjust, mkSMap)
import HasFix.TH     (AnnDict)

import Parenthesize.Utils

-- Current limitations:
--   * No support for comments


parensDemoQ :: TH.QuasiQuoter
parensDemoQ = TH.QuasiQuoter makeExpr undefined undefined undefined
  where
    makeExpr code = do
      result <- parenthesizeCode (undefined :: ExpS) code
      TH.lift (code ++ "-- Result:" ++ result ++ "\n\n" ++ replicate 80 '-')

{- TODO (separate module? Would need haskell-src-meta dependency)

-- | Just like 'parensDemoQ', except it also tests that the output re-parses
--   and evaluates to the same result.
parensTestQ :: TH.QuasiQuoter
parensTestQ = TH.QuasiQuoter makeExpr undefined undefined undefined
  where
    makeExpr code = do
        code' <- parenthesizeCode (undefined :: ExpS) code
        text <- TH.lift (code ++ "\n-- Result:\n" ++ code' ++ "\n" ++ replicate 80 '-')
        either (TH.lift . show)  . parseResultToEither <$> parseExpWithMode parseExtsMode code'
      where
-}

parensMainQ :: TH.QuasiQuoter
parensMainQ = TH.QuasiQuoter undefined undefined undefined makeMain
  where
    makeMain code = mkMain =<< TH.lift =<< parenthesizeCode (undefined :: DecS) code
    mkMain expr = return [TH.FunD (TH.mkName "main") [TH.Clause [] (TH.NormalB expr) []]]

-- | Given a string of haskell code, parenthesize according to the in-scope
--   fixities.  First parameter is used to specify which AST type to use.
parenthesizeCode :: forall a t.
  ( Data a, SWC.Data (AnnDict SrcSpanInfo) a, Parseable a
  , a ~ t SrcSpanInfo, ExactP t, AppFixity t
  ) => a -> String -> TH.Q String
parenthesizeCode _ code
  = either show (flip exactPrint [])
  . fmap parenthesize
  . parseResultToEither
  <$> (parseWithFixities parseExtsMode code :: TH.Q (ParseResult a))

-- | Utility synonym for the types of helper functions inside 'parenthesize'.
type SubstWriter = Writer [(SrcSpan, String)]

-- | Parenthesizes all expressions found inside the specified AST.  Infix
--   operators are assumed to already be appropriately fixitied.
parenthesize :: (Data a, SWC.Data (AnnDict SrcSpanInfo) a) => a -> a
parenthesize input = adjust (mkSMap $ Map.fromListWith (++) subs) output
  where
    (output, subs) = runWriter $ outside input

    -- Adds parenthesis outside and inside
    outside :: Data a => a -> SubstWriter a
    outside = inside `extM` doExpr

    -- Adds parenthesis inside
    inside :: Data a => a -> SubstWriter a
    inside = gmapM outside

    doExpr :: ExpS -> SubstWriter ExpS
    doExpr (App l a@(App _ _ _) b)    = App l <$> inside a <*> inside b
    doExpr (App l a             b)    = App l <$> outside a <*> inside b

    doExpr (InfixApp l a op@(QVarOp opLoc (UnQual _ (Symbol _ sym))) b)
    -- Special Case: turn $ into parens
      | sym == "$" = App l a <$> outside b

    -- Special Case: add an extra space to "-" when needed.
      | sym == "-" && isNumber (leftmostNoSep b) && spanAdj opLoc (ann b) = do
        let (SrcSpanInfo (SrcSpan file _ _ el ec) _) = opLoc
        tell [(SrcSpan file el ec el ec, " ")]
        giveParen $ InfixApp l a op b

    -- Add parens to each entry that's filled.
    -- Two extra 'gmapM's
    doExpr (TupleSection l   xs)      = TupleSection l <$> mapM (mapM inside) xs

    doExpr (List         l   xs)      = List  l <$> mapM inside xs
    doExpr (Tuple        l   xs)      = Tuple l <$> mapM inside xs
    doExpr (ParComp      l e xs)      = ParComp  l <$> inside e <*> mapM inside xs
    doExpr (ListComp     l e xs)      = ListComp l <$> inside e <*> mapM inside xs

    -- Add parens to the sub-part of the nested expression, because these are
    -- all things that already look like grouping expressions.
    doExpr x@(Paren _ _)              = gmapM (gmapM inside) x
    doExpr x@(EnumFrom _ _)           = gmapM (gmapM inside) x
    doExpr x@(EnumFromTo _ _ _)       = gmapM (gmapM inside) x
    doExpr x@(EnumFromThen _ _ _)     = gmapM (gmapM inside) x
    doExpr x@(EnumFromThenTo _ _ _ _) = gmapM (gmapM inside) x
    doExpr x@(BracketExp _ _)         = gmapM (gmapM inside) x
    doExpr x@(SpliceExp _ _)          = gmapM (gmapM inside) x

    -- Add parens inside sections, but not outside.
    doExpr x@(LeftSection _ _ _)      = inside x
    doExpr x@(RightSection _ _ _)     = inside x

    -- Leafs that don't look like they need parens.
    doExpr x@(Lit _ _)                = return x
    doExpr x@(Var _ _)                = return x
    doExpr x@(QuasiQuote _ _ _)       = return x

    -- The default case is to add parenthesis!
    doExpr x = giveParen x

{-
    doType (AppT l a@(AppT _ _ _) b) = AppT l <$> inside a <*> inside b
    doType (AppT l a b)              = AppT l <$> outside a <*> inside b
-}

    giveParen x = do
      let (SrcSpanInfo sp@(SrcSpan file sl sc el ec) _) = ann x
          is = [ SrcSpan file sl sc sl (sc+1)
               , SrcSpan file el (ec-1) el ec
               ]
          
      tell [ (SrcSpan file sl (sc+1) sl (sc+1), "(")
           , (SrcSpan file el ec el ec, ")")
           ]
      return . Paren (SrcSpanInfo sp is) =<< inside x

-- | 
parseWithFixities :: forall a t.
  ( a ~ t SrcSpanInfo, AppFixity t
  , Parseable a, Data a
  ) => ParseMode -> String -> TH.Q (ParseResult a)
parseWithFixities mode code = case parseWithMode mode code of
    ParseOk (parsed :: a) -> do
        mfs <- mapM getFixity =<< infixNames
        let fs = catMaybes mfs
        ParseOk <$> applyFixities fs parsed
      where
        infixNames
            = fmap catMaybes
            . mapM (either
                (\n -> fmap (n,) <$> TH.lookupTypeName  (showName n))
                (\n -> fmap (n,) <$> TH.lookupValueName (showName n)))
            . Set.toList . Set.fromList
            . map sQName
            . execWriter $ everywhereM getInfix parsed
    err -> return err
  where
    getFixity (en, tn) = fmap (extsFixity (sQName en)) <$> lookupFixity tn
    showName = filter (`notElem` "()") . prettyPrint
    getInfix :: Data b => b -> Writer [Either QNameS QNameS] b
    getInfix = return
      `extM` getInfixA
      `extM` getInfixE
      `extM` getInfixM
      `extM` getInfixO
      `extM` getInfixT
    getInfixA x@(InfixA     _ _ n _)     = tell [Left  n]  >> return x
    getInfixA x                          =                    return x
    getInfixE x@(PInfixApp  _ _ n _)     = tell [Right n]  >> return x
    getInfixE x                          =                    return x
    getInfixM x@(InfixMatch _ _ n _ _ _) = tell [Right qn] >> return x where qn = UnQual (ann n) n
    getInfixM x                          =                    return x
    getInfixO x@(QConOp       _ n)       = tell [Right n]  >> return x
    getInfixO x@(QVarOp       _ n)       = tell [Right n]  >> return x
    getInfixO x                          =                    return x
    getInfixT x@(TyInfix    _ _ n _)     = tell [Left  n]  >> return x
    getInfixT x                          =                    return x

extsFixity :: HSE.QName -> TH.Fixity -> HSE.Fixity
extsFixity n (TH.Fixity v dir) = HSE.Fixity (extsAssoc dir) v n
  where
    extsAssoc TH.InfixL = HSE.AssocLeft
    extsAssoc TH.InfixR = HSE.AssocRight
    extsAssoc TH.InfixN = HSE.AssocNone

--NOTE: this is the part of the program that could be error-prone.
lookupFixity :: TH.Name -> TH.Q (Maybe TH.Fixity)
lookupFixity n = TH.recover notFound (process =<< TH.reify n)
  where
    process (TH.ClassOpI _ _ _ f) = return $ Just f
    process (TH.DataConI _ _ _ f) = return $ Just f
    process (TH.VarI     _ _ _ f) = return $ Just f
    process _ = notFound
    notFound = do
      TH.reportWarning $ "No fixity information found for " ++ TH.pprint n
      return Nothing
