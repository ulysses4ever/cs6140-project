{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveGeneric #-}


module Stringify (toTreeMatches, Tree(..), rdrNameToStr) where

-- =====================================================
-- imports
-- =====================================================

-- GHC
import BasicTypes
import HsExtension
import HsExpr
import HsSyn
import RdrName
import SrcLoc
import Bag

-- Datatype programming
import Data.Data hiding (Fixity)
import OccName hiding (occName)
import Module

-- Pretty printing
import Text.PrettyPrint.GenericPretty

-- Standard
import Data.Maybe

--import qualified Data.ByteString as B

-- =====================================================
-- code
-- =====================================================

-- A string tree to represent an AST
data Tree =
  Leaf String |
  Node String [Tree]
  deriving (Show, Generic, Eq)

instance Out Tree

toTreeMatches :: [Match GhcPs (LHsExpr GhcPs)] -> Tree
toTreeMatches mts = Node "FunDefMatch" $ map toTreeMatch mts

toTreeMatch :: Match GhcPs (LHsExpr GhcPs) -> Tree
toTreeMatch mt = astToTree mt

-- -----------------------------------------------------
-- main definition
-- -----------------------------------------------------

astToTree :: Data a => a -> Tree
astToTree = 
  generic 
    `ext1Q` list
    `extQ` string --`extQ` bytestring
    `extQ` hsValBindsLR 
    `extQ` gRHSs `extQ` gRHS
    `extQ` pat `extQ` lit
    `extQ` matchGroup `extQ` match `extQ` hsMatchContext 
    `extQ` hsOverLit
    `extQ` rdrName `extQ` occName
    `ext2Q` located

generic :: Data a => a -> Tree
generic t = 
  let 
    children = filter good (gmapQ astToTree t)
    name     = showConstr (toConstr t)
  in
    case children of
      [] -> Leaf name
      _  -> Node name children
  where 
    good (Leaf "NoExt")   = False
    good (Node "NoExt" _) = False
    good (Node "List" []) = False
    good (Leaf "EmptyLocalBinds") = False
    good (Leaf "WpHole") = False
    good (Node "SyntaxExpr" _) = False
    good _ = True

-- -----------------------------------------------------
-- interesting AST cases
-- -----------------------------------------------------

hsValBindsLR :: HsValBindsLR GhcPs GhcPs -> Tree
hsValBindsLR (ValBinds _X bnds _LSig) = 
  Node "ValBinds" $ map astToTree bndsList
  where 
    bndsList = bagToList bnds
hsValBindsLR _ = error "panic: HsValBindsLR unknown constructor"


gRHS :: GRHS GhcPs (LHsExpr GhcPs) -> Tree
gRHS (GRHS _X gs bd) = Node "GRHSs" chs
  where 
    bdTree = astToTree bd
    chs = case gs of
            []   -> [bdTree]
            gs'' -> [Node "GuardLStmtList" $ map astToTree gs'', bdTree] 
gRHS _ = error "panic: GRHS unknown constructor"

gRHSs :: GRHSs GhcPs (LHsExpr GhcPs) -> Tree
gRHSs (GRHSs _ rhs binds) = Node "GRHSs" $ rhs' ++ binds' 
  where 
    binds' = case unLoc binds of
      EmptyLocalBinds _ -> []
      HsValBinds _X vbs -> [hsValBindsLR vbs]
      _ -> [astToTree binds]
    rhs' = case rhs of
            []   -> []
            gs'' -> [Node "GRHSList" $ map astToTree gs'']
gRHSs _ = error "panic: GRHSs unknown constructor"

lit :: HsLit GhcPs -> Tree
lit (HsString st _) = Leaf "HsString" --Node "HsString" [Leaf (sourceTextToStr st)]
lit l = generic l

pat :: Pat GhcPs -> Tree
pat (NPat _X (L _ hsLit) neg _eq) =
  case hsLit of
    OverLit _ val _ -> 
      Leaf $ (maybe "" (const "-") neg) ++ overLitValToStr val
    _ -> error "panic"
pat (NPlusKPat {}) = Leaf "NPlusKPat"
pat (WildPat _) = Leaf "XWildPat"
pat (VarPat _ p) = Node "VarPat" [astToTree p]
pat (LazyPat _ p) = Node "LazyPat" [astToTree p]
pat (AsPat _ name p) = Node "AsPat" [astToTree name, astToTree p]
pat (ParPat _ p) = Node "ParPat" [astToTree p]
pat (BangPat _ p) = Node "BangPat" [astToTree p]
pat (ListPat _ p) = Node "ListPat" (map astToTree p)
pat (TuplePat _ p _b) = Node "TuplePat" (map astToTree p) 
pat (SumPat _ p t _) = Node "SumPat" [astToTree p, astToTree t]
pat (ConPatIn name p) = Node "ConPatIn" [astToTree name, astToTree p]
pat pt@(ConPatOut {}) = Node "ConPatOut" (gmapQ astToTree pt)
pat (ViewPat _ l p) = Node "ViewPat" [astToTree l, astToTree p]
pat (SplicePat _ p) = Node "SplicePat" [astToTree p]
pat (LitPat _ p) = Node "LitPat" [astToTree p]
pat (SigPat _ p) = Node "SigPat" [astToTree p]
pat (CoPat _ w p t) = Node "CoPat" [astToTree w, astToTree p, astToTree t]
pat (XPat _) = Leaf "XPat" 

hsOverLit :: HsOverLit GhcPs -> Tree
hsOverLit (OverLit _ val _) = Leaf $ overLitValToStr val
hsOverLit _ = error "panic: HsOverLit unknown constructor"

overLitValToStr :: OverLitVal -> String
overLitValToStr (HsIntegral (IL txt _ _)) = sourceTextToStr txt
overLitValToStr (HsFractional (FL txt _ _)) = sourceTextToStr txt
overLitValToStr (HsIsString txt _) = sourceTextToStr txt

sourceTextToStr :: SourceText -> String
sourceTextToStr (SourceText str) = str
sourceTextToStr NoSourceText = "NoSourceText"

matchGroup :: MatchGroup GhcPs (LHsExpr GhcPs) -> Tree
matchGroup (MG _X matches _orig) = Node "MatchGroup" $ map astToTree (unLoc matches)
matchGroup _ = error "panic: MatchGroup unknown constructor"

match :: Match GhcPs (LHsExpr GhcPs) -> Tree
match (Match _ ctx pats rhs) = Node "Match" $
    hsMatchContext ctx
    : pats' 
    ++ [astToTree rhs]
  where
    pats' = case pats of
      [] -> []
      _ -> [Node "Pats" (map astToTree pats)]
match _ = error "panic: Match unknown constructor"

hsMatchContext :: HsMatchContext (NameOrRdrName (IdP GhcPs)) -> Tree
hsMatchContext (FunRhs (L _ name) _ _) = 
  Node "FunRhs" [Leaf (rdrNameToStr name)]
hsMatchContext mc = Leaf (showConstr (toConstr mc))

-- -----------------------------------------------------
-- simple AST cases
-- -----------------------------------------------------

list :: Data a => [a] -> Tree
list xs = Node "List" $ map astToTree xs

string :: String -> Tree
string s = Leaf $ "%%%" ++ s

--bytestring :: B.ByteString -> Tree
--bytestring s = Leaf $ "%%%" ++ show s

occName :: OccName -> Tree
occName n  =  Leaf $ "%%%" ++ (OccName.occNameString n)

rdrName :: RdrName -> Tree
rdrName = Leaf . rdrNameToStr

rdrNameToStr :: RdrName -> String
rdrNameToStr n = (OccName.occNameString (rdrNameOcc n))


located :: (Data b) => GenLocated loc b -> Tree
located (L _ a) = astToTree a




{-
************************************************************************
*                                                                      *
* Copied from syb
*                                                                      *
************************************************************************
-}

-- | The type constructor for queries
newtype Q q x = Q { unQ :: x -> q }

-- | Extend a generic query by a type-specific case
extQ :: ( Typeable a
        , Typeable b
        )
     => (a -> q)
     -> (b -> q)
     -> a
     -> q
extQ f g a = maybe (f a) g (cast a)

-- | Type extension of queries for type constructors
ext1Q :: (Data d, Typeable t)
      => (d -> q)
      -> (forall e. Data e => t e -> q)
      -> d -> q
ext1Q def ext = unQ ((Q def) `ext1` (Q ext))


-- | Type extension of queries for type constructors
ext2Q :: (Data d, Typeable t)
      => (d -> q)
      -> (forall d1 d2. (Data d1, Data d2) => t d1 d2 -> q)
      -> d -> q
ext2Q def ext = unQ ((Q def) `ext2` (Q ext))

-- | Flexible type extension
ext1 :: (Data a, Typeable t)
     => c a
     -> (forall d. Data d => c (t d))
     -> c a
ext1 def ext = maybe def id (dataCast1 ext)



-- | Flexible type extension
ext2 :: (Data a, Typeable t)
     => c a
     -> (forall d1 d2. (Data d1, Data d2) => c (t d1 d2))
     -> c a
ext2 def ext = maybe def id (dataCast2 ext)
