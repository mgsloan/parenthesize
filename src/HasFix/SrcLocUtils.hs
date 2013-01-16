-- Utilities to work with SrcLoc, SrcSpan and SrcSpanInfo types
{-# LANGUAGE TemplateHaskell #-}
module HasFix.SrcLocUtils where

import Data.Lens.Common
import Data.Lens.Template
import Control.Applicative
import Language.Haskell.Exts.SrcLoc

type Line = Int
type Col = Int

-- Derive lenses for SrcSpanInfo and SrcSpan. The lens names are the field names
-- followed by "L".
concat <$> mapM (flip nameMakeLens (Just . (++"L"))) [''SrcSpanInfo, ''SrcSpan, ''SrcLoc]

srcSpanStartL, srcSpanEndL :: Lens SrcSpan SrcLoc
srcSpanStartL = lens
  (\span -> SrcLoc
    (srcSpanFilename span)
    (srcSpanStartLine span)
    (srcSpanStartColumn span))
  (\(SrcLoc f l c) span -> span
    { srcSpanFilename = f
    , srcSpanStartLine = l
    , srcSpanStartColumn = c
    })
srcSpanEndL = lens
  (\span -> SrcLoc
    (srcSpanFilename span)
    (srcSpanEndLine span)
    (srcSpanEndColumn span))
  (\(SrcLoc f l c) span -> span
    { srcSpanFilename = f
    , srcSpanEndLine = l
    , srcSpanEndColumn = c
    })

-- NB this doesn't satisfy the set/get lens law
-- (but is still useful...)
srcSpanToInfo :: Lens SrcSpan SrcSpanInfo
srcSpanToInfo = iso (\s -> SrcSpanInfo s []) srcInfoSpan

mapSpans :: (SrcSpan -> SrcSpan) -> SrcSpanInfo -> SrcSpanInfo
mapSpans f = modL srcInfoSpanL f . modL srcInfoPointsL (map f)
