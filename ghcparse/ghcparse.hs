{-#LANGUAGE BangPatterns #-}
import GHC
import ErrUtils
import FastString
import Lexer
import Parser
import Outputable
import SrcLoc
import StringBuffer
import SysTools
import GHC.Paths ( libdir )
import DynFlags ( initDynFlags, defaultDynFlags )

import Data.List (partition)
import System.Environment (getArgs)
import System.IO
import System.FilePath.Find as Find
    (extension, fileName, find, (&&?),
     (/~?), (==?))

import Control.Monad.Parallel (mapM)

main :: IO ()
main = do
    args <- getArgs
    hsFiles <- findHsSources (head args)
    dflags <- initDflags
    !res <- Control.Monad.Parallel.mapM (parse dflags) hsFiles
    -- !res <- Prelude.mapM parse hsFiles
    let (succR, _) = partition (== True) res
        totalFiles   = length res
        rate = fromIntegral (length succR) / 
               fromIntegral totalFiles
    putStrLn $ "Total files:.......... " ++ show totalFiles
    putStrLn $ "Parser success rate:.. " ++ show (rate :: Double)

parse :: DynFlags -> String -> IO Bool
parse dflags file = do
    con <- myReadFile file
    let !pres = runParser dflags file con Parser.parseModule
    printParseRes dflags file pres
    return $ presToBool pres

runParser :: DynFlags -> String -> String -> P a -> ParseResult a
runParser flags filename str parser = unP parser parseState
    where
      location = mkRealSrcLoc (mkFastString filename) 1 1
      buf = stringToStringBuffer str
      parseState = mkPState flags buf location

initDflags :: IO DynFlags
initDflags = do
    let ldir = Just libdir
    mySettings <- initSysTools ldir
    myLlvmConfig <- initLlvmConfig ldir
    initDynFlags (defaultDynFlags mySettings myLlvmConfig)

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
  Outputable a =>
  DynFlags -> String -> ParseResult a -> IO ()
printParseRes _dflags _ (POk _state _res) = --print $
  --showSDoc dflags $ ppr res
  return ()
printParseRes dflags file (PFailed _ _ msg) = do
  print "*************************************************"
  print file
  putMsg dflags msg
  putStrLn ""
