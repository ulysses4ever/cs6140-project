{-#LANGUAGE TupleSections #-}
{-#LANGUAGE BangPatterns #-}

module Process where

-- =====================================================
-- imports
-- =====================================================

import Prelude --hiding (id)
import Text.Casing
import Data.Hashable (hash)

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

thisFuncName :: String
thisFuncName = "THIS_FUNCTION"

-- Function declaration
data FunDecl = 
    FunDecl {
      fd_id      :: IdP GhcPs,
      fd_matches :: [Match GhcPs (LHsExpr GhcPs)]
    } 

hsDeclsToTree :: [LHsDecl GhcPs] -> [Tree]
hsDeclsToTree ds = map funDeclToTree (getFunDecls ds)

processHsDecls :: Bool -> Int -> Int -> Int -> [LHsDecl GhcPs] -> [String]
processHsDecls useHash mn mx num ds = pathStrs
  where
    fds = getFunDecls ds
    pathStrs = map (processFunDecl useHash mn mx num) fds

processFunDecl :: Bool -> Int -> Int -> Int -> FunDecl -> String
processFunDecl useHash mn mx num fd@(FunDecl name _) = 
    ident ++ " " ++ pathsStr
  where
    nameStr = rdrNameToStr name
    tokens = unIdentifier $ fromAny nameStr
    ident  = map toLower $ intercalate "|" tokens
    tree   = funDeclToTree fd
    paths  = genPathsInRange mn mx num tree
    !pathsStr = intercalate " " $ map (showPath useHash) paths

-- -----------------------------------------------------
-- process Tree
-- -----------------------------------------------------

-- path to a leaf
type LeafInfo = [String]
-- one Code2Vec path: paths from each of the leaves
type C2VPath = (LeafInfo, LeafInfo)

{-
escapeThisFuncName :: String -> C2VPath -> C2VPath
escapeThisFuncName name (l1, r, l2, len) =
    (l1', r, l2', len)
  where
    l1' = escape l1
    l2' = escape l2
    escape n | n == name = thisFuncName
             | otherwise = n
-}

showPath :: Bool -> C2VPath -> String
showPath _ ([], _) = error "showPath: empty half-path, left"
showPath _ (_, []) = error "showPath: empty half-path, right"
showPath useHash (_ : s1, s2) = intercalate ","
    [ x1'
    ,  path'
    , x2'
    ]
  where
    (s2', x2) = splitLast s2
    s1' = reverse s1
    s1'' = tail s1'
    x1 = head s1'
    path = intercalate "^" $
      filter
      (not . null)
      [showHalfPath '^' s1'',
        showHalfPath '_' s2']
    path' = if useHash then show $ hash path else path
    escape ',' = '&'
    escape '\n' = '*'
    escape ' ' = '%'
    escape c = c
    x1' = map escape x1
    x2' = map escape x2

showHalfPath :: Char -> [String] -> String
showHalfPath sep xs = intercalate [sep] xs

splitLast :: [a] -> ([a], a)
splitLast [] = error "splitLast: empty list"
splitLast [x] = ([], x)
splitLast (x:xs) =
   let (xs', lastx) = splitLast xs in (x:xs', lastx)

type DList a = [a] -> [a]

type C2VInfoD = ([LeafInfo], DList C2VPath)

genPaths :: Tree -> [C2VPath]
genPaths t = forceD $ snd $ genPaths' t

genPathsInRange :: Int -> Int -> Int -> Tree -> [C2VPath]
genPathsInRange mn mx num t = take num $ filter good $ genPaths t
  where
    good (s1, s2) = (mn <= len) && (len <= mx) where
      len = length s1 + length s2 - 1

genPaths' :: Tree -> C2VInfoD
genPaths' (Leaf s)    = ([[s]], id)
genPaths' (Node s []) = ([[s]], id)
genPaths' (Node s ts) = (newLeafs, paths . newPaths)
  where
    -- leafs and paths for subtrees
    infos :: [C2VInfoD]
    infos = map genPaths' ts

    leafInfos :: [[LeafInfo]]
    leafInfos = map fst infos

    paths :: DList C2VPath
    paths = concatCD $ map snd infos

    makePath :: LeafInfo -> LeafInfo -> C2VPath
    makePath s1 s2 = (s : s1, s : s2)

    makePaths :: [LeafInfo] -> [LeafInfo] -> DList C2VPath
    makePaths ls1 ls2 = fromList $ biLoop ls1 ls2 makePath
    
    newPaths :: DList C2VPath
    newPaths =  (triLoop leafInfos makePaths :: DList C2VPath)
    
    newLeafs :: [LeafInfo]
    newLeafs =
      [ s:leaf
      | nodeInfo <- leafInfos
      , leaf <- nodeInfo
      ]


triLoop :: [a] -> (a -> a -> DList b) -> DList b
triLoop []  _f = id
triLoop (x : xs) f = (concatCD [f x y | y <- xs]) . (triLoop xs f)

fromList :: [a] -> DList a
fromList xs = (xs ++)

singletonD :: a -> DList a
singletonD = (:)

biLoop :: [a] -> [b] -> (a -> b -> c) -> [c]
biLoop xs ys f = [f x y | x <- xs, y <- ys]

forceD :: DList a -> [a]
forceD xs = xs []

concatCD :: [DList a] -> DList a
concatCD xs = foldl (\acc -> \info -> acc . info) id xs

--------------------------------------------------------------------------
--               Examples
--------------------------------------------------------------------------

ex1 :: Tree
ex1 = Node "X" [Leaf "a"
               , Node "Y" [Leaf "b", Leaf "c"]
               , Node "E" []
               , Node "Z" [Leaf "p", Node "W" [Leaf "q", Leaf "s"]]]

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
