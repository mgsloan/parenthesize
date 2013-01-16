{-# LANGUAGE TemplateHaskell #-}
module HasFix.TH where
import qualified Language.Haskell.Exts.Annotated as HSE
import Language.Haskell.Exts.Annotated (SrcSpanInfo)
import qualified Language.Haskell.Modules as HM
import Data.Lens.Common
import Language.Haskell.TH

import Data.Generics.SYB.WithClass.Basics

-- This list is obtained with the following command:
-- perl -lne '/instance Annotated (\S+)/ && print ", '"''"'$1";' src/Language/Haskell/Exts/Annotated/Syntax.hs
annTypes, notAnnTypes :: [Name]
annTypes = 
  [ ''HSE.ModuleName
  , ''HSE.SpecialCon
  , ''HSE.QName
  , ''HSE.Name
  , ''HSE.IPName
  , ''HSE.QOp
  , ''HSE.Op
  , ''HSE.CName
  , ''HSE.Module
  , ''HSE.ModuleHead
  , ''HSE.ExportSpecList
  , ''HSE.ExportSpec
  , ''HSE.ImportDecl
  , ''HSE.ImportSpecList
  , ''HSE.ImportSpec
  , ''HSE.Assoc
  , ''HSE.Deriving
  , ''HSE.Decl
  , ''HSE.Annotation
  , ''HSE.DataOrNew
  , ''HSE.DeclHead
  , ''HSE.InstHead
  , ''HSE.Binds
  , ''HSE.IPBind
  , ''HSE.Match
  , ''HSE.QualConDecl
  , ''HSE.ConDecl
  , ''HSE.FieldDecl
  , ''HSE.GadtDecl
  , ''HSE.ClassDecl
  , ''HSE.InstDecl
  , ''HSE.BangType
  , ''HSE.Rhs
  , ''HSE.GuardedRhs
  , ''HSE.Type
  , ''HSE.TyVarBind
  , ''HSE.Kind
  , ''HSE.FunDep
  , ''HSE.Context
  , ''HSE.Asst
  , ''HSE.Literal
  , ''HSE.Exp
  , ''HSE.XName
  , ''HSE.XAttr
  , ''HSE.Bracket
  , ''HSE.Splice
  , ''HSE.Safety
  , ''HSE.CallConv
  , ''HSE.ModulePragma
  , ''HSE.Activation
  , ''HSE.Rule
  , ''HSE.RuleVar
  , ''HSE.WarningText
  , ''HSE.Pat
  , ''HSE.PXAttr
  , ''HSE.RPatOp
  , ''HSE.RPat
  , ''HSE.PatField
  , ''HSE.Stmt
  , ''HSE.QualStmt
  , ''HSE.FieldUpdate
  , ''HSE.Alt
  , ''HSE.GuardedAlts
  , ''HSE.GuardedAlt
  ]

notAnnTypes =
  [ ''SrcSpanInfo
  , ''HSE.SrcSpan
  , ''HSE.SrcLoc
  , ''HSE.Boxed
  , ''HSE.Tool
  , ''HSE.Comment
  , ''HM.Scoped
  , ''HM.Msg
  , ''HM.MsgArg
  , ''HM.MsgLevel
  ]

data AnnDict l a
  = Annotated { annotation :: Lens a l }
  | NotAnnotated

annDict :: HSE.Annotated a => AnnDict l (a l)
annDict = Annotated $ lens HSE.ann (\s a -> HSE.amap (const s) a)

mkAnnSatInstance :: Name -> Q [Dec]
mkAnnSatInstance name = do
  l <- newName "l"
  sequence [ instanceD (return [])
    (conT ''Sat
      `appT` (
        conT ''AnnDict
          `appT` varT l
          `appT` (conT name `appT` varT l))) [method] ]
  where
  method = funD 'dict [clause [] (normalB (varE 'annDict)) []]
