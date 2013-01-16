{-# LANGUAGE FlexibleContexts #-}
module HasFix.Adjust (SMap, adjust, mkSMap) where

-- standard modules
import Prelude hiding (id, (.), (!!))
import qualified Data.Map as Map
import Data.Maybe
import Control.Monad
import Control.Applicative
-- lenses
import Control.Category
import Data.Lens.Common
-- syb-with-class
import Data.Generics.SYB.WithClass.Basics
import Data.Generics.SYB.WithClass.Instances ()
-- haskell-src-exts
import Language.Haskell.Exts.Annotated
-- local modules
import HasFix.SrcLocUtils
import HasFix.GenericUtils

type ShiftAmount = Int

type SMap = Map.Map Line (Map.Map Col ShiftAmount)

mkSMap :: Map.Map SrcSpan String -> SMap
mkSMap rmap = Map.fromListWith Map.union $ do
  (span, rep) <- Map.toList rmap
  -- start and end lines are always the same for identifiers... right?
  let line = span^.srcSpanStartLineL
      col  = span^.srcSpanEndColumnL
      amount = newLen - oldLen
      newLen = length rep
      -- spans are half-open, so no +1
      oldLen = span^.srcSpanEndColumnL - span^.srcSpanStartColumnL
  return (line, Map.singleton col amount)

findBlocks :: Data (AnnDict SrcSpanInfo) ast => ast -> [SrcSpan]
findBlocks ast =
  maybeToList (recognizeBlock ast) ++
  concat (gmapQ spanInfoProxy findBlocks ast)

blocksToSMap :: [SrcSpan] -> SMap -> SMap
-- warning: accumulating thunks
blocksToSMap blocks origSMap = foldl addBlock origSMap blocks
  where
  addBlock :: SMap -> SrcSpan -> SMap
  addBlock smap block =
    -- calculate the shift of the first item
    -- NB: using the original SMap to calculate the shift here -- otherwise
    -- nested blocks will be shifted multiple times
    let firstShift = calculateShift origSMap $ block^.srcSpanStartL
    -- and shift accordingly all the lines of the block starting from the next one
        lines = [srcSpanStartLine block + 1 .. srcSpanEndLine block]
    in foldl (shiftLine firstShift) smap lines

  shiftLine :: Int -> SMap -> Int -> SMap
  shiftLine amount smap line =
    Map.insertWith (Map.unionWith (+)) line (Map.singleton 0 amount) smap

recognizeBlock :: Data (AnnDict SrcSpanInfo) ast => ast -> Maybe SrcSpan
recognizeBlock ast = recognizeDo <|> recognizeCase <|> recognizeBinds
  where
  recognizeDo :: Maybe SrcSpan
  recognizeDo = do
    exp <- cast ast
    (ann, stmts) <-
      case exp of
        Do  ann stmts -> Just (ann, stmts)
        MDo ann stmts -> Just (ann, stmts)
        _ -> Nothing
    -- check that the subtree is formatted with layout, not braces
    brace <- srcInfoPoints ann !! 1
    guard $ isSpanEmpty brace
    processItems stmts

  recognizeCase :: Maybe SrcSpan
  recognizeCase = do
    Case ann _ alts <- cast ast
    brace <- srcInfoPoints ann !! 2
    guard $ isSpanEmpty brace
    processItems alts

  -- This covers where and let blocks
  recognizeBinds :: Maybe SrcSpan
  recognizeBinds = do
    binds <- cast ast
    brace <- srcInfoPoints (ann binds) !! 0
    guard $ isSpanEmpty brace
    case binds of
      BDecls  _ decls -> processItems $ decls
      IPBinds _ binds -> processItems $ binds

  processItems :: Data (AnnDict SrcSpanInfo) ast => [ast] -> Maybe SrcSpan
  processItems [] = Nothing
  processItems items = do
    ann <- annotationL
    let start = head items ^. (srcInfoSpanL . ann)
        end = last items ^. (srcInfoSpanL . ann)
    return $ setL srcSpanEndL (end ^. srcSpanEndL) start

adjust :: Data (AnnDict SrcSpanInfo) ast => SMap -> ast -> ast
adjust smap ast =
  let blocks = findBlocks ast
      smap' = blocksToSMap blocks smap

      go :: Data (AnnDict SrcSpanInfo) ast => ast -> ast
      go = gmapT spanInfoProxy go . shiftAst smap'

  in go ast

isSpanEmpty :: SrcSpan -> Bool
isSpanEmpty span = span^.srcSpanStartL == span^.srcSpanEndL

calculateShift :: SMap -> SrcLoc -> ShiftAmount
calculateShift smap loc = fromMaybe 0 $ do
  shifts <- Map.lookup (srcLine loc) smap
  let after_col = fst $ Map.split (srcColumn loc + 1) shifts -- (<= col) equiv. to (< col+1)
  return $ Map.foldl' (+) 0 after_col

-- shift* functions adjust position of tokens that happened to
-- be on the same line as and to the right of a modified token

shiftAst :: Data (AnnDict SrcSpanInfo) ast => SMap -> ast -> ast
shiftAst smap ast = fromMaybe ast $ do
  lens <- annotationL
  return $ modL lens (shiftSpanInfo smap) ast

shiftSpanInfo :: SMap -> SrcSpanInfo -> SrcSpanInfo
shiftSpanInfo smap = mapSpans $ shiftSpan smap

shiftSpan :: SMap -> SrcSpan -> SrcSpan
shiftSpan smap span =
  foldr (\l -> modL l (shiftLoc smap)) span [srcSpanStartL, srcSpanEndL]

shiftLoc :: SMap -> SrcLoc -> SrcLoc
shiftLoc smap loc = modL srcColumnL (+ calculateShift smap loc) loc

-- safe list indexing
(!!) :: [a] -> Int -> Maybe a
l !! n = listToMaybe $ drop n l
