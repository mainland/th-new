{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Language.Haskell.TH.Compile where

import Control.Exception   (bracket)
import Control.Monad       (void)
import Data.Typeable       (Typeable,
                            typeOf)
import Language.Haskell.TH (TExp,
                            unType,
                            pprint, Dec(ValD), Pat(VarP), mkName, Body(NormalB))
import System.Directory    (removeFile)
import System.FilePath     (addExtension,
                            splitExtension)
import System.IO           (hClose,
                            hFlush,
                            hPutStr,
                            openTempFile)
import Unsafe.Coerce       (unsafeCoerce)

import GHC hiding (ValD)
import GHC.Paths (libdir)
import DynFlags  (defaultFatalMessager,
                  defaultFlushOut)

compile :: forall a . Typeable a => TExp a -> IO a
compile texp =
  bracket (open texp) cleanup $ \tfile -> do
    defaultErrorHandler defaultFatalMessager defaultFlushOut $ do
    runGhc (Just libdir) $ do
    dflags <- getSessionDynFlags
    void $ setSessionDynFlags dflags
    target <- guessTarget tfile Nothing
    setTargets [target]
    r <- load LoadAllTargets
    case r of
      Failed    -> error "Compilation failed"
      Succeeded -> do
        setContext [IIDecl $ simpleImportDecl (mkModuleName "TempMod")]
        result <- compileExpr ("TempMod.myFunc")
        let result' = unsafeCoerce result :: a
        return result'
  where
    open :: TExp a -> IO FilePath
    open e = do
        (tfile, h) <- openTempFile "." "TempMod.hs"
        hPutStr h $ unlines
           [ "module TempMod where"
           , "import GHC.Num"
           , ""
           , "myFunc :: " ++ show (typeOf (undefined :: a))
           , pprint $ ValD (VarP (mkName "myFunc")) (NormalB (unType e)) []
           ]
        hFlush h
        hClose h
        return tfile

    cleanup :: FilePath -> IO ()
    cleanup path = do
        removeFile path
        removeFile (addExtension root ".hi")
        removeFile (addExtension root ".o")
      where
        root :: FilePath
        (root, _) = splitExtension path
