{-# LANGUAGE RankNTypes #-}

module Stringify (toTreeMatches, Tree(..)) where

-- =====================================================
-- imports
-- =====================================================

-- GHC
import HsExtension
import HsExpr
import HsSyn

import SrcLoc

-- Datatype programming
import Data.Data hiding (Fixity)
import OccName hiding (occName)

--import qualified Data.ByteString as B

-- =====================================================
-- code
-- =====================================================

-- A string tree to represent an AST
data Tree = Leaf String | Node String [Tree] deriving (Show)

toTreeMatches :: [Match GhcPs (LHsExpr GhcPs)] -> Tree
toTreeMatches mts = Node "FunDefMatch" $ map toTreeMatch mts

toTreeMatch :: Match GhcPs (LHsExpr GhcPs) -> Tree
toTreeMatch mt = astToTree mt


astToTree :: Data a => a -> Tree
astToTree = 
  generic 
    `extQ` string --`extQ` bytestring
    `ext1Q` list
    `extQ` occName
    `ext2Q` located

generic :: Data a => a -> Tree
generic t = Node (showConstr (toConstr t))
                 (filter bad (gmapQ astToTree t))
  where 
    bad (Node "NoExt" _) = False
    bad _ = True


                
list :: Data a => [a] -> Tree
list xs = Node "&&&list" $ map astToTree xs

string :: String -> Tree
string s = Leaf $ "%%%" ++ s

--bytestring :: B.ByteString -> Tree
--bytestring s = Leaf $ "%%%" ++ show s

occName n  =  Leaf $ "%%%" ++ (OccName.occNameString n)


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