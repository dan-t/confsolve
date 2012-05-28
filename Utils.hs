{-# LANGUAGE ScopedTypeVariables, OverloadedStrings #-}

module Utils where
import System.IO
import System.Environment
import Data.Time.Clock
import Data.Time.Calendar
import Control.Exception.Base (try)
import qualified Data.Text as T
import qualified Filesystem as FS
import qualified Filesystem.Path.CurrentOS as FP
import Filesystem.Path ((</>), (<.>))

errorsToStderr :: IO () -> IO ()
errorsToStderr action =
   catch action (\e -> appPutStrLn stderr (show e))

appPutStrLn :: Handle -> String -> IO ()
appPutStrLn handle string = do
   progName <- normalizedProgName
   hPutStrLn handle ("\n" ++ T.unpack progName ++ ": " ++ string)

normalizedProgName :: IO T.Text
normalizedProgName = do
   pn <- getProgName
   return (T.pack $ takeWhile (/= '.') pn)

appDataDirectory = do
   pn <- normalizedProgName
   hd <- FS.getHomeDirectory
   return $ hd </> FP.fromText (T.append "." pn)

trashDirectory = do
   ad <- appDataDirectory
   return $ ad </> "trash"

defaultDiff = "gvimdiff -f"

getCurrentDate :: IO (Integer,Int,Int) -- :: (year,month,day)
getCurrentDate = getCurrentTime >>= return . toGregorian . utctDay

getEnvOrDefault :: String -> String -> IO String
getEnvOrDefault envVar defaultValue = do
   result <- try $ getEnv envVar
   case result of
	Right value          -> return value
	Left  (_ :: IOError) -> return defaultValue

toText :: FP.FilePath -> T.Text
toText filePath =
   either (\text -> error $ "Couldn't decode FilePath '" ++ show filePath ++ "' to Text!")
          id
          (FP.toText filePath)

toString :: FP.FilePath -> String
toString = T.unpack . toText

-- splits filePath into (directory, baseName, extension)
splitFilePath :: FP.FilePath -> (FP.FilePath, FP.FilePath, T.Text)
splitFilePath filePath = (dir, bname, ext)
   where
      dir   = FP.directory filePath
      bname = FP.basename filePath
      ext   = allExtensions filePath

allExtensions :: FP.FilePath -> T.Text
allExtensions = (T.intercalate ".") . FP.extensions

replaceBaseName :: FP.FilePath -> FP.FilePath -> FP.FilePath
replaceBaseName filePath newBaseName = dir </> newBaseName <.> ext
   where
      dir = FP.directory filePath
      ext = allExtensions filePath
