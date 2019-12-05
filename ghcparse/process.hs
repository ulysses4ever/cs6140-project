{-#LANGUAGE TupleSections #-}

module Process where

-- =====================================================
-- imports
-- =====================================================

import Prelude hiding (id)
import Text.Casing

-- GHC
import HsDecls
import HsExtension
import HsExpr
import HsBinds
import SrcLoc

-- Standard
import Data.Maybe
import Data.List
import Data.Char

-- Project
import Stringify

-- =====================================================
-- code
-- =====================================================

-- Function declaration
data FunDecl = 
    FunDecl {
      fd_id      :: IdP GhcPs,
      fd_matches :: [Match GhcPs (LHsExpr GhcPs)]
    } 

hsDeclsToTree :: [LHsDecl GhcPs] -> [Tree]
hsDeclsToTree ds = map funDeclToTree (getFunDecls ds)

processHsDecls :: Int -> Int -> [LHsDecl GhcPs] -> [String]
processHsDecls min max ds = pathStrs
  where
    fds = getFunDecls ds
    pathStrs = map (processFunDecl min max) fds

processFunDecl :: Int -> Int -> FunDecl -> String
processFunDecl min max fd@(FunDecl id _) = 
  ident ++ " " ++ pathsStr
  where
    tokens = unIdentifier $ fromAny (rdrNameToStr id)
    ident  = map toLower $ intercalate "|" tokens
    tree   = funDeclToTree fd
    paths  = genPathsInRange min max tree
    pathsStr = intercalate " " $ map showPath paths

-- -----------------------------------------------------
-- process Tree
-- -----------------------------------------------------

-- leaf-string and length of the path to the leaf
type LeafInfo = (String, Int)

type C2VPath = (String, String, String, Int)

type C2VInfo = ([LeafInfo], [C2VPath])

genPaths :: Tree -> [C2VPath]
genPaths t = snd $ genPaths' 0 t

genPathsInRange :: Int -> Int -> Tree -> [C2VPath]
genPathsInRange min max t = filter good $ genPaths t
  where
    good (_,_,_,len) = (min <= len) && (len <= max)


showPath :: C2VPath -> String
showPath (l1, r, l2, _n) = l1 ++ "," ++ r ++ "," ++ l2

genPaths' :: Int -> Tree -> C2VInfo
genPaths' len t@(Leaf s)    = ([(s, len)], [])
genPaths' len t@(Node s []) = ([(s, len)], [])
genPaths' len t@(Node s ts) = (newLeafs, paths ++ newPaths)
  where
    -- leafs and paths for subtrees
    infos :: [C2VInfo]
    infos = map (\t -> genPaths' (len+1) t) ts
    leafInfos :: [[LeafInfo]]
    leafInfos = map fst infos
    paths :: [C2VPath]
    paths = concatMap snd infos
    makePath :: LeafInfo -> LeafInfo -> C2VPath
    makePath (s1, n1) (s2, n2) = (s1, s, s2, n1+n2-len-len)
    makePaths :: [LeafInfo] -> [LeafInfo] -> [C2VPath]
    makePaths ls1 ls2 = biLoop ls1 ls2 makePath
    newPaths :: [C2VPath]
    newPaths = concat $ triLoop leafInfos makePaths
    newLeafs :: [LeafInfo]
    newLeafs = concat leafInfos

    -- for ( :: C2VInfo in infos

triLoop :: [a] -> (a -> a -> b) -> [b]
triLoop []  _f = []
triLoop (x : xs) f = [f x y | y <- xs] 
    ++ triLoop xs f

biLoop :: [a] -> [b] -> (a -> b -> c) -> [c]
biLoop xs ys f = [f x y | x <- xs, y <- ys]

--triLoop xs ys f = concatMap (\x -> zipWith f (repeat x) ys) xs
{-triLoop [] _ys _f     = []
triLoop _xs [] _f     = []
triLoop (x : xs) ys f = map (\y -> f x y) ys ++
    triLoop xs ys f
-}

ex1 = Node "X" [Leaf "a", Node "Y" [Leaf "b", Leaf "c"], 
                Node "Z" [Leaf "p", Node "W" [Leaf "q", Leaf "s"]]]

ex1' = Node "W" [Leaf "q", Leaf "s"]
ex2' = Node "Z" [Leaf "p", ex1']

{-
ex2 = Leaf "a"

ex3 = Node "X" []

ex4 = Node "X" [Node "Y" []]

type Path = [Tree]
-- path to a leaf: (list of nodes, length)
type LPath = (Path, Int)


commonPath :: Path -> Path -> LPath
commonPath xs ys = (reverse path, len)
  where
    (path, len) = commonPath' ([], 0) xs ys

-- takes common path so far and two paths to consider
commonPath' :: LPath -> Path -> Path -> LPath
commonPath' comp [] _ = comp
commonPath' comp _ [] = comp
commonPath' comp@(path, len) (x : xs) (y : ys) = 
  if x == y 
  then commonPath' (x : path, len + 1) xs ys
  else comp

getLeafs :: Tree -> [LPath]
getLeafs t = map (\(path, len) -> (reverse path, len)) $ getLeafs' ([], 0) t

-- takes a path to a node and the node
getLeafs' :: LPath -> Tree -> [LPath]
getLeafs' (path, len) t@(Leaf _)    = [(t : path, len + 1)]
getLeafs' (path, len) t@(Node _ []) = [(t : path, len + 1)]
getLeafs' (path, len) t@(Node _ ts) = concat leafPaths
  where 
    path' = t : path
    len'  = len + 1 
    leafPaths = map (\t' -> getLeafs' (path', len') t') ts

showPathList :: [LPath] -> String
showPathList ps = intercalate "\n" $ map showPath ps

showPath :: LPath -> String
showPath (ts, len) = "Path(" ++ show len ++ "|" ++ showTreeList ts ++ ")"

showTreeList :: [Tree] -> String
showTreeList [] = ""
showTreeList (t : ts) = 
  (case t of
     Leaf s   -> s
     Node s _ -> s)
  ++ ',' : showTreeList ts
-}

-- -----------------------------------------------------
-- process AST
-- -----------------------------------------------------

funDeclToTree :: FunDecl -> Tree
funDeclToTree (FunDecl id mts) =
  Node (rdrNameToStr id) [toTreeMatches mts]

getFunDecls :: [LHsDecl GhcPs] -> [FunDecl]
getFunDecls = catMaybes . map getFunDecl

-- returns binding if it's a function bind
getFunDecl :: LHsDecl GhcPs -> Maybe FunDecl
getFunDecl (L _ (ValD _ (
      FunBind 
        _ 
        (L _ id)
        (MG _ (L _ alts) _) 
        _
        _
    ))) = 
    Just (FunDecl id (map unLoc alts))
getFunDecl _ = Nothing
