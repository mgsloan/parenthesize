-- Utilities for generic AST traversals and for working with annotations
-- generically.
{-# LANGUAGE
      FlexibleContexts,
      FlexibleInstances,
      MultiParamTypeClasses,
      UndecidableInstances,
      TemplateHaskell,
      OverlappingInstances
  #-}
module HasFix.GenericUtils
  ( AnnDict(..)
  , spanInfoProxy
  , scopedSpanProxy
  , annotationL
  )
where

import Prelude hiding (id, (.))
import Control.Category
import Data.Lens.Common
import Language.Haskell.Exts.Annotated
import Language.Haskell.Modules
import Control.Applicative
import Data.Generics.SYB.WithClass.Derive
import Data.Generics.SYB.WithClass.Basics
import HasFix.TH
import HasFix.SrcLocUtils

spanInfoProxy :: Proxy (AnnDict SrcSpanInfo)
spanInfoProxy = error "proxy"

scopedSpanProxy :: Proxy (AnnDict (Scoped SrcSpan))
scopedSpanProxy  = error "proxy"

deriveData (annTypes ++ notAnnTypes)

-- Default implementation
instance Sat (AnnDict l a) where
  dict = NotAnnotated

concat <$> mapM mkAnnSatInstance annTypes

-- We treat Comment as annotated as well (although it's not a member of the
-- Annotated class)
instance Sat (AnnDict SrcSpanInfo Comment) where
  dict = Annotated $
    srcSpanToInfo .
    lens (\(Comment _ span _) -> span)
         (\span (Comment b _ s) -> Comment b span s)

annotationL :: Sat (AnnDict l a) => Maybe (Lens a l)
annotationL =
  case dict of
    NotAnnotated -> Nothing
    Annotated l -> Just l
