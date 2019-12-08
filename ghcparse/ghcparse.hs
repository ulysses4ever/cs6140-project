{-#LANGUAGE BangPatterns #-}
{-#LANGUAGE ScopedTypeVariables #-}
{-#LANGUAGE RecordWildCards #-}

{-#OPTIONS -Wno-unused-top-binds #-}

import GHC hiding (parser)
import qualified GHC.LanguageExtensions as LangExt
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
    parseDynamicFilePragma, xopt )


import qualified ApiAnnotation as GHC
import qualified DynFlags      as GHC
import qualified FastString    as GHC
import qualified GHC           as GHC hiding (parseModule)
import qualified HeaderInfo    as GHC
import qualified Lexer         as GHC
import qualified MonadUtils    as GHC
import qualified Outputable    as GHC
import qualified Parser        as GHC
import qualified SrcLoc        as GHC
import qualified StringBuffer  as GHC

import qualified Bag            as GHC
import qualified DriverPhases   as GHC
import qualified DriverPipeline as GHC
import qualified DynFlags       as GHC
import qualified ErrUtils       as GHC
import qualified FastString     as GHC
import qualified HscTypes       as GHC
import qualified Lexer          as GHC

import Data.Data
import Data.List hiding (find)
import Data.Maybe
import System.Environment (getArgs)
import System.IO
import Control.Exception (catch)
import Control.Monad (unless)
import System.FilePath.Find as Find
    (extension, fileName, find, (&&?),
     (/~?), (==?))

import Control.Monad.Parallel (mapM)

-- Pretty printing
import Text.PrettyPrint.GenericPretty

import Process
import Stringify

myPrint :: [Tree] -> IO ()
myPrint =
  ppLen 80
--   putStrLn . show

main :: IO ()
main = do
    args <- getArgs
    let wd = head args
        is_stdout = length args > 1
    hsFiles <- findHsSources wd
    !dflags <- initDflags
    -- !res <- Control.Monad.Parallel.mapM (parse dflags) hsFiles
    !res <- Prelude.mapM (parse dflags is_stdout) hsFiles
    let (succR, _) = partition (== True) res
        totalFiles   = length res
        rate = fromIntegral (length succR) / 
               fromIntegral totalFiles
    unless is_stdout $ do
      putStrLn $ "Total files:.......... " ++ show totalFiles
      putStrLn $ "Parser success rate:.. " ++ show (rate :: Double)

parse :: DynFlags -> Bool -> String -> IO Bool
parse dflags0 is_stdout file = do
    buf <- hGetStringBuffer file
    dflags1 <- augmentDflags dflags0 buf file

    -- CPP
    let useCpp = xopt LangExt.Cpp dflags1
    (!fileContents, dflags2) <-
      if useCpp
      then
        ghcWrapper $ getPreprocessedSrcDirect dflags1 file
      else do
        --txt <- GHC.liftIO $ readFileGhc file
        -- let (contents1,lp) = stripLinePragmas txt
        -- return (contents1,lp,dflags)
        return (buf, dflags1)

    (dflags3, _, _) <-
          parseDynamicFlagsCmdLine dflags2 [GHC.noLoc "-hide-all-packages"]

    let !pres = runParser dflags3 file fileContents Parser.parseModule -- cost of this bang is high
    printParseRes dflags1 is_stdout file pres
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
    return dflags1

-- ---------------------------------------------------------------------
data CppOptions = CppOptions
                { cppDefine :: [String]    -- ^ CPP #define macros
                , cppInclude :: [FilePath] -- ^ CPP Includes directory
                , cppFile :: [FilePath]    -- ^ CPP pre-include file
                }

defaultCppOptions :: CppOptions
defaultCppOptions = CppOptions [] [] []

getPreprocessedSrcDirect :: (GhcMonad m)
                         => DynFlags
                         -> FilePath
                         -> m (StringBuffer, GHC.DynFlags)
getPreprocessedSrcDirect dynFlags src =
  GHC.setSessionDynFlags dynFlags >>
  getPreprocessedSrcDirectPrim src

getPreprocessedSrcDirectPrim :: (GhcMonad m)
                              => FilePath
                              -> m (StringBuffer, DynFlags)
getPreprocessedSrcDirectPrim src_fn = do
  hsc_env <- getSession
  let odf = GHC.hsc_dflags hsc_env
  let normal = do
      (dflags', hspp_fn) <- GHC.preprocess hsc_env (src_fn, Just (GHC.Cpp GHC.HsSrcFile))
      buf <- GHC.hGetStringBuffer hspp_fn
      return (buf, dflags')
  
  GHC.liftIO $ catch
    normal
    (\(_::GhcException) -> return (stringToStringBuffer "", odf))


showErrorMessages :: GHC.ErrorMessages -> String
showErrorMessages msgs = intercalate "\n" $ map show $ GHC.bagToList msgs

injectCppOptions :: CppOptions -> GHC.DynFlags -> GHC.DynFlags
injectCppOptions CppOptions{..} dflags =
  foldr addOptP dflags (map mkDefine cppDefine ++ map mkIncludeDir cppInclude ++ map mkInclude cppFile)
  where
    mkDefine = ("-D" ++)
    mkIncludeDir = ("-I" ++)
    mkInclude = ("-include" ++)


addOptP :: String -> GHC.DynFlags -> GHC.DynFlags
addOptP   f = alterSettings (\s -> s { GHC.sOpt_P   = f : GHC.sOpt_P s})

alterSettings :: (GHC.Settings -> GHC.Settings) -> GHC.DynFlags -> GHC.DynFlags
alterSettings f dflags = dflags { GHC.settings = f (GHC.settings dflags) }


readFileGhc :: FilePath -> IO String
readFileGhc file = do
  buf@(GHC.StringBuffer _ len _) <- GHC.hGetStringBuffer file
  return (GHC.lexemeToString buf len)

-- | Internal function. Default runner of GHC.Ghc action in IO.
ghcWrapper :: GHC.Ghc a -> IO a
ghcWrapper =
  GHC.defaultErrorHandler GHC.defaultFatalMessager GHC.defaultFlushOut
    . GHC.runGhc (Just libdir)
  
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------


-- | A Haskell comment. The @AnnKeywordId@ is present if it has been converted
-- from an @AnnKeywordId@ because the annotation must be interleaved into the
-- stream and does not have a well-defined position
data Comment = Comment
    {
      commentContents   :: !String -- ^ The contents of the comment including separators

    -- AZ:TODO: commentIdentifier is a misnomer, should be commentSrcSpan, it is
    -- the thing we use to decide where in the output stream the comment should
    -- go.
    , commentIdentifier :: !GHC.SrcSpan -- ^ Needed to uniquely identify two comments with the same contents
    , commentOrigin     :: !(Maybe GHC.AnnKeywordId) -- ^ We sometimes turn syntax into comments in order to process them properly.
    }

mkComment :: String -> GHC.SrcSpan -> Comment
mkComment c ss = Comment c ss Nothing

-- | Remove GHC style line pragams (@{-# LINE .. #-}@) and convert them into comments.
stripLinePragmas :: String -> (String, [Comment])
stripLinePragmas = unlines' . unzip . findLines . lines
  where
    unlines' (a, b) = (unlines a, catMaybes b)

findLines :: [String] -> [(String, Maybe Comment)]
findLines = zipWith checkLine [1..]

checkLine :: Int -> String -> (String, Maybe Comment)
checkLine line s
  |  "{-# LINE" `isPrefixOf` s =
       let (pragma, res) = getPragma s
           size   = length pragma
           mSrcLoc = mkSrcLoc (mkFastString "LINE")
           ss     = mkSrcSpan (mSrcLoc line 1) (mSrcLoc line (size+1))
       in (res, Just $ mkComment pragma ss)
  -- Deal with shebang/cpp directives too
  -- x |  "#" `isPrefixOf` s = ("",Just $ Comment ((line, 1), (line, length s)) s)
  |  "#!" `isPrefixOf` s =
    let mSrcLoc = mkSrcLoc (mkFastString "SHEBANG")
        ss = mkSrcSpan (mSrcLoc line 1) (mSrcLoc line (length s))
    in
    ("",Just $ mkComment s ss)
  | otherwise = (s, Nothing)

getPragma :: String -> (String, String)
getPragma [] = error "Input must not be empty"
getPragma s@(x:xs)
  | "#-}" `isPrefixOf` s = ("#-}", "   " ++ drop 3 s)
  | otherwise =
      let (prag, remline) = getPragma xs
      in (x:prag, ' ':remline)
----------------------------------------------------------------------------

presToBool :: ParseResult a -> Bool
presToBool (POk{}) = True
presToBool (PFailed{}) = False

findHsSources :: FilePath -> IO [FilePath]
findHsSources fp = find isVisible (isHsFile &&? isVisible) fp
    where
      isHsFile = Find.extension ==? ".hs"
      isVisible = fileName /~? ".?*"

printParseRes ::
  DynFlags ->
  Bool ->   -- use stdout
  String -> -- file name
  ParseResult (Located (HsModule GhcPs)) ->
  IO ()
printParseRes _dflags is_stdout fname (POk _state (L _ res)) = do
  --myPrint $
  --  hsDeclsToTree (hsmodDecls res)
  let !cts = intercalate "\n" $ processHsDecls 0 30 400 (hsmodDecls res)
  if is_stdout
    then putStrLn cts
    else writeFile (fname ++ ".l30paths") cts
         
  -- putStrLn "================================================="
  --putStrLn $  -- remove `_` from `_dflags` in arg-list
  --  showSDoc dflags $ showAstData NoBlankSrcSpan res
printParseRes dflags _is_stdout file (PFailed _ _ msg) = do
  print "*************************************************"
  print file
  putMsg dflags msg
  putStrLn ""
