{-# LANGUAGE ScopedTypeVariables #-}

module Utils where
import System.IO
import System.Directory
import System.Environment
import System.FilePath
import Data.List
import Data.Time.Clock
import Data.Time.Calendar
import Control.Exception.Base (try)

getDirContents dir = do
   entries <- getDirectoryContents dir
   return $ filter notDots entries
   where
      notDots entry = not $ "." == entry || ".." == entry

errorsToStderr :: IO () -> IO ()
errorsToStderr action =
   catch action (\e -> do pn <- normalizedProgName
			  hPutStrLn stderr ("\n" ++ pn ++ ": " ++ show e))

normalizedProgName = do
   pn <- getProgName
   return $ takeWhile (/= '.') pn


appDirectory   = normalizedProgName >>= \pn -> getAppUserDataDirectory pn
trashDirectory = appDirectory >>= \d -> return $ d </> "trash" 

defaultDiff = "gvimdiff -f"

getCurrentDate :: IO (Integer,Int,Int) -- :: (year,month,day)
getCurrentDate = getCurrentTime >>= return . toGregorian . utctDay

getEnvOrDefault :: String -> String -> IO String
getEnvOrDefault envVar defaultValue = do
   result <- try $ getEnv envVar
   case result of
	Right value          -> return value
	Left  (_ :: IOError) -> return defaultValue
