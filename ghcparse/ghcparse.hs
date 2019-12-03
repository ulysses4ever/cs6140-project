{-#LANGUAGE BangPatterns #-}
{-#LANGUAGE ScopedTypeVariables #-}

import GHC hiding (parser)
import ErrUtils
import FastString
import HeaderInfo
import HscTypes
import HsDumpAst
import Lexer
import Parser
import Outputable
import SrcLoc
import StringBuffer hiding (buf)
import SysTools
import GHC.Paths ( libdir )
import DynFlags
  ( initDynFlags, defaultDynFlags, parseDynamicFlagsCmdLine,
    parseDynamicFilePragma )

import Data.Data
import Data.List (partition)
import System.Environment (getArgs)
import System.IO
import Control.Exception (catch)
import System.FilePath.Find as Find
    (extension, fileName, find, (&&?),
     (/~?), (==?))

import Control.Monad.Parallel (mapM)

-- Pretty printing
import Text.PrettyPrint.GenericPretty

import Process
import Stringify

myPrint :: Tree -> IO ()
myPrint =
  ppLen 80
  -- putStrLn . show

main :: IO ()
main = do
    args <- getArgs
    hsFiles <- findHsSources (head args)
    !dflags <- initDflags
    !res <- Control.Monad.Parallel.mapM (parse dflags) hsFiles
    -- !res <- Prelude.mapM (parse dflags) hsFiles
    let (succR, _) = partition (== True) res
        totalFiles   = length res
        rate = fromIntegral (length succR) / 
               fromIntegral totalFiles
    putStrLn $ "Total files:.......... " ++ show totalFiles
    putStrLn $ "Parser success rate:.. " ++ show (rate :: Double)

parse :: DynFlags -> String -> IO Bool
parse dflags0 file = do
    buf <- hGetStringBuffer file
    dflags1 <- augmentDflags dflags0 buf file
    --let dflags1 = dflags0
    let !pres = runParser dflags1 file buf Parser.parseModule -- cost of this bang is high
    printParseRes dflags1 file pres
    return $ presToBool pres

runParser :: DynFlags -> String -> StringBuffer -> P a -> ParseResult a
runParser flags path buf parser = unP parser parseState
    where
      location = mkRealSrcLoc (mkFastString path) 1 1
      parseState = mkPState flags buf location

-- account for lang extensions
augmentDflags :: DynFlags -> StringBuffer -> FilePath -> IO DynFlags
augmentDflags dflags0 buf path = do
  let src_opts    =  getOptions dflags0 buf path
  (dflags1, _, _) <- catch
      (parseDynamicFilePragma dflags0 src_opts)
      (\(_::SourceError) -> return (dflags0, undefined, undefined))
  return dflags1

initDflags :: IO DynFlags
initDflags = do
    let ldir = Just libdir
    mySettings <- initSysTools ldir
    myLlvmConfig <- initLlvmConfig ldir
    dflags1 <- initDynFlags (defaultDynFlags mySettings myLlvmConfig)
    (dflags2, _, _) <-
      parseDynamicFlagsCmdLine dflags1 [GHC.noLoc "-hide-all-packages"]
    return dflags2

presToBool :: ParseResult a -> Bool
presToBool (POk{}) = True
presToBool (PFailed{}) = False

findHsSources :: FilePath -> IO [FilePath]
findHsSources fp = find isVisible (isHsFile &&? isVisible) fp
    where
      isHsFile = Find.extension ==? ".hs"
      isVisible = fileName /~? ".?*"

myReadFile :: String -> IO String
myReadFile file= do
  hd <- openFile file ReadMode
  hSetBinaryMode hd True
  hGetContents hd

printParseRes ::
--  (Data a) =>
--  DynFlags -> String -> ParseResult a -> IO ()
  DynFlags -> String -> ParseResult (Located (HsModule GhcPs)) -> IO ()
printParseRes dflags _ (POk _state (L _ res)) = do
  myPrint $
    hsDeclsToTree (hsmodDecls res)
  putStrLn "================================================="
  --putStrLn $
  --  showSDoc dflags $ showAstData NoBlankSrcSpan res
printParseRes dflags file (PFailed _ _ msg) = do
  print "*************************************************"
  print file
  putMsg dflags msg
  putStrLn ""
