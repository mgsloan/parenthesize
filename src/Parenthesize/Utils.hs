module Parenthesize.Utils where

import Language.Haskell.Exts.Annotated

type ExpS = Exp SrcSpanInfo
type DecS = Decl SrcSpanInfo
type QNameS = QName SrcSpanInfo

parseExtsMode :: ParseMode
parseExtsMode = ParseMode "" knownExtensions True False Nothing

parseMode :: ParseMode
parseMode = ParseMode "" [] False False Nothing

parseModeWithFixities :: ParseMode -> [Fixity] -> ParseMode
parseModeWithFixities p fs = p { fixities = Just fs }

-- | Copied from haskell-src-meta, "Language.Haskell.Meta.Parse"
parseResultToEither :: ParseResult a -> Either String a
parseResultToEither (ParseOk a) = Right a
parseResultToEither (ParseFailed loc e)
  = let line = srcLine loc - 1
    in Left (unlines [show line,show loc,e])


-- All of the following is just used to detect the case that subtraction should
-- get another space..

spanAdj :: SrcSpanInfo -> SrcSpanInfo -> Bool
spanAdj (SrcSpanInfo (SrcSpan _ _ _ el ec) _)
        (SrcSpanInfo (SrcSpan _ sl sc _ _) _)
  = (el == sl) && (ec == sc)

isNumber :: ExpS -> Bool
isNumber (Lit _ x) = isNumberLit x
isNumber _ = False

isNumberLit :: Literal SrcSpanInfo -> Bool
isNumberLit (Int  _ _ _)       = True
isNumberLit (Frac _ _ _)       = True
isNumberLit (PrimInt _ _ _)    = True
isNumberLit (PrimWord _ _ _)   = True
isNumberLit (PrimFloat _ _ _)  = True
isNumberLit (PrimDouble _ _ _) = True
isNumberLit _                  = False

-- | The leftmost AST subtree (all other AST nodes require leading delimiters).
leftmostNoSep :: ExpS -> ExpS
leftmostNoSep (InfixApp        _ x _ _) = leftmostNoSep x
leftmostNoSep (App             _ x _)   = leftmostNoSep x
leftmostNoSep (LeftArrApp      _ x _)   = leftmostNoSep x
leftmostNoSep (RightArrApp     _ x _)   = leftmostNoSep x
leftmostNoSep (LeftArrHighApp  _ x _)   = leftmostNoSep x
leftmostNoSep (RightArrHighApp _ x _)   = leftmostNoSep x
leftmostNoSep x = x