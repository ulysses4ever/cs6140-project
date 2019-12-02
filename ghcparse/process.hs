module Process where

-- GHC
import HsDecls
import HsExtension
import HsExpr
import HsBinds
import SrcLoc

-- Standard
import Data.Maybe

-- Project
import Stringify



-- Function declaration
data FunDecl = 
    FunDecl {
      fd_id      :: IdP GhcPs,
      fd_matches :: [Match GhcPs (LHsExpr GhcPs)]
    } 



foo :: [LHsDecl GhcPs] -> Tree
foo ds = let fds = getFunDecls ds in
    case fds of
        [] -> Leaf "-"
        (FunDecl _ mts) : _ -> toTreeMatches mts


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