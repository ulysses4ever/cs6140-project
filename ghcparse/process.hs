{-#LANGUAGE TupleSections #-}
{-#LANGUAGE BangPatterns #-}

module Process where

-- =====================================================
-- imports
-- =====================================================

import Prelude --hiding (id)
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

processHsDecls :: Int -> Int -> Int -> [LHsDecl GhcPs] -> [String]
processHsDecls mn mx num ds = pathStrs
  where
    fds = getFunDecls ds
    pathStrs = map (processFunDecl mn mx num) fds

processFunDecl :: Int -> Int -> Int -> FunDecl -> String
processFunDecl mn mx num fd@(FunDecl name _) = 
  ident ++ " " ++ pathsStr
  where
    tokens = unIdentifier $ fromAny (rdrNameToStr name)
    ident  = map toLower $ intercalate "|" tokens
    tree   = funDeclToTree fd
    paths  = genPathsInRange mn mx num tree
    !pathsStr = intercalate " " $ map showPath paths

-- -----------------------------------------------------
-- process Tree
-- -----------------------------------------------------

-- leaf-string and length of the path to the leaf
type LeafInfo = (String, Int)
-- one Code2Vec path with its length
type C2VPath = (String, String, String, Int)

showPath :: C2VPath -> String
showPath (l1, r, l2, _n) = 
    (map s l1) ++ "," ++ (map s r) ++ "," ++ (map s l2)
   where
    s ' ' = '_'
    s ',' = '#'
    s c = c
    

type DList a = [a] -> [a]

type C2VInfoD = (DList LeafInfo, DList C2VPath)

genPaths :: Tree -> [C2VPath]
genPaths t = forceD $ snd $ genPaths' 0 t

genPathsInRange :: Int -> Int -> Int -> Tree -> [C2VPath]
genPathsInRange mn mx num t = take num $ filter good $ genPaths t
  where
    good (_,_,_,len) = (mn <= len) && (len <= mx)

genPaths' :: Int -> Tree -> C2VInfoD
genPaths' len (Leaf s)    = (((s, len):), id)
genPaths' len (Node s []) = (((s, len):), id)
genPaths' len (Node s ts) = (newLeafs, paths . newPaths)
  where
    -- leafs and paths for subtrees
    infos :: [C2VInfoD]
    infos = map (\t -> genPaths' (len+1) t) ts

    leafInfosD :: [DList LeafInfo]
    leafInfosD = map fst infos

    leafInfos :: [[LeafInfo]]
    leafInfos = map forceD leafInfosD

    paths :: DList C2VPath
    paths = concatCD $ map snd infos

    makePath :: LeafInfo -> LeafInfo -> C2VPath
    makePath (s1, n1) (s2, n2) = (s1, s, s2, n1+n2-len-len)

    makePaths :: [LeafInfo] -> [LeafInfo] -> DList C2VPath
    makePaths ls1 ls2 = fromList $ biLoop ls1 ls2 makePath
    
    newPaths :: DList C2VPath
    newPaths =  (triLoop leafInfos makePaths :: DList C2VPath)
    
    newLeafs :: DList LeafInfo
    newLeafs = concatCD leafInfosD

triLoop :: [a] -> (a -> a -> DList b) -> DList b
triLoop []  _f = id
triLoop (x : xs) f = (concatCD [f x y | y <- xs]) . (triLoop xs f)

fromList :: [a] -> DList a
fromList xs = (xs ++)

biLoop :: [a] -> [b] -> (a -> b -> c) -> [c]
biLoop xs ys f = [f x y | x <- xs, y <- ys]

forceD :: DList a -> [a]
forceD xs = xs []

concatCD :: [DList a] -> DList a
concatCD xs = foldl (\acc -> \info -> acc . info) id xs

--mapD :: (a -> b) -> DList a -> DList b
--mapD f = foldr ((x:) . f) id

--concatDC :: DList [a] -> DList a
--concatDC 

--concatDD :: DList (DList a) -> DList a

--concat :: List l => l [a] -> l a
--concat = concatDD . liftM fromList

{-
type C2VInfo = ([LeafInfo], [C2VPath])

genPaths :: Tree -> [C2VPath]
genPaths t = snd $ genPaths' 0 t

genPathsInRange :: Int -> Int -> Tree -> [C2VPath]
genPathsInRange mn mx t = filter good $ genPaths t
  where
    good (_,_,_,len) = (mn <= len) && (len <= mx)


showPath :: C2VPath -> String
showPath (l1, r, l2, _n) = l1 ++ "," ++ r ++ "," ++ l2

genPaths' :: Int -> Tree -> C2VInfo
genPaths' len (Leaf s)    = ([(s, len)], [])
genPaths' len (Node s []) = ([(s, len)], [])
genPaths' len (Node s ts) = (newLeafs, paths ++ newPaths)
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

triLoop :: [a] -> (a -> a -> b) -> [b]
triLoop []  _f = []
triLoop (x : xs) f = [f x y | y <- xs] 
    ++ triLoop xs f

biLoop :: [a] -> [b] -> (a -> b -> c) -> [c]
biLoop xs ys f = [f x y | x <- xs, y <- ys]
-}

--biLoop xs ys f = concatMap (\x -> zipWith f (repeat x) ys) xs
{-
biLoop [] _ys _f     = []
biLoop _xs [] _f     = []
biLoop (x : xs) ys f = map (\y -> f x y) ys ++
    biLoop xs ys f
-}

ex1 :: Tree
ex1 = Node "X" [Leaf "a", Node "Y" [Leaf "b", Leaf "c"], 
                Node "Z" [Leaf "p", Node "W" [Leaf "q", Leaf "s"]]]

ex1' :: Tree
ex1' = Node "W" [Leaf "q", Leaf "s"]
ex2' :: Tree
ex2' = Node "Z" [Leaf "p", ex1']

{-
ex2 :: Tree
ex2 = Leaf "a"

ex3 :: Tree
ex3 = Node "X" []

ex4 :: Tree
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
funDeclToTree (FunDecl name mts) =
  Node (rdrNameToStr name) [toTreeMatches mts]

getFunDecls :: [LHsDecl GhcPs] -> [FunDecl]
getFunDecls = catMaybes . map getFunDecl

-- returns binding if it's a function bind
getFunDecl :: LHsDecl GhcPs -> Maybe FunDecl
getFunDecl (L _ (ValD _ (
      FunBind 
        _ 
        (L _ name)
        (MG _ (L _ alts) _) 
        _
        _
    ))) = 
    Just (FunDecl name (map unLoc alts))
getFunDecl _ = Nothing
