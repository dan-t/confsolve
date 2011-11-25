{-# LANGUAGE ScopedTypeVariables #-}

import System (getArgs)
import System.Directory
import System.IO
import System.IO.Error (IOErrorType)
import System.FilePath
import System.Environment
import System.Process
import System.Exit
import Data.List (isInfixOf, isSuffixOf, isPrefixOf, concat, foldr)
import Data.Char (intToDigit, digitToInt, isDigit, toUpper)
import Data.Foldable (foldrM, foldlM)
import qualified Data.HashSet as HS
import Control.Monad (mapM_)
import Foreign.Marshal.Error (void)
import qualified FileConflict as FC
import qualified Dropbox as DB
import qualified Wuala as WU
import Utils

main = do
   hSetBuffering stdout NoBuffering
   hSetBuffering stdin  NoBuffering
   args <- getArgs
   case args of
	[]            -> printHelp
        ("-h":[])     -> printHelp >> printRuntineHelp
	("--help":[]) -> printHelp >> printRuntineHelp
	("-d":dir:[]) -> void $ resolve DB.DropboxConflict dir HS.empty
	("-w":dir:[]) -> void $ resolve WU.WualaConflict dir HS.empty
	otherwise     -> error $ "Invalid Arguments!"

printHelp = do
   putStrLn $ ""
   putStrLn $ "Usage: confsolve OPTION DIRECTORY"
   putStrLn $ ""
   putStrLn $ "Options:"
   putStrLn $ "   -d   resolve Dropbox file conflicts"
   putStrLn $ "   -w   resolve Wuala file conflicts"
   putStrLn $ "   -h   print this help message"
   putStrLn $ ""

printRuntineHelp = do
   trashDir <- trashDirectory
   putStrLn $ ""
   putStrLn $ "Runtime Options:"
   putStrLn $ "   (T)ake File (NUM) => By pressing 't' and a digit, the conflicting file with the"
   putStrLn $ "                        digit NUM is used as the new version. A copy of the"
   putStrLn $ "                        current file and the other conflicting files is put"
   putStrLn $ "                        into the trash directory '" ++ trashDir ++ "'."
   putStrLn $ ""
   putStrLn $ "   (M)ove to Trash   => By pressing 'm', all conflicting files are"
   putStrLn $ "                        moved into the trash directory '" ++ trashDir ++ "'."
   putStrLn $ ""
   putStrLn $ "   Show (D)iff (NUM) => By pressing 'd' and a digit, the difference between the"
   putStrLn $ "                        current file and the conflicting file NUM is shown."
   putStrLn $ "                        If there's only one conflicting file, then only pressing"
   putStrLn $ "                        'd' is sufficient."
   putStrLn $ "                        By pressing 'd' and two digits, the difference between"
   putStrLn $ "                        the two conflicting files is shown."
   putStrLn $ "                        The diff tool can be specified by the user by setting the environment"
   putStrLn $ "                        variable 'CONFSOLVE_DIFF'. The default diff tool is 'gvimdiff -f'."
   putStrLn $ ""
   putStrLn $ "   (S)kip            => By pressing 's', the current conflict is skipped"
   putStrLn $ "                        and the next one is shown."
   putStrLn $ ""
   putStrLn $ "   (Q)uit            => By pressing 'q', the application is quit."
   putStrLn $ ""
   putStrLn $ "   (H)elp            => By pressing 'h', this help is printed."
   putStrLn $ ""

type Resolved = HS.Set String

resolve :: (FC.FileConflict a) => a -> String -> Resolved -> IO Resolved
resolve fileConflict file resolved = do
   dirExists <- doesDirectoryExist file
   if dirExists
      then do
	 entries <- getDirContents file
	 foldrM (\e r -> resolve fileConflict (file </> e) r) resolved entries
      else do
	 fileExists <- doesFileExist file
	 let isntResolved = not $ file `HS.member` resolved
	 if fileExists && isntResolved
	    then do
	       maybeConfInfo <- FC.find fileConflict file
	       case maybeConfInfo of
		    Nothing       -> return resolved
		    Just confInfo -> do
		       handleConflict confInfo
		       return $ foldr (\c r -> HS.insert (FC.filePath c) r) resolved (FC.conflicts confInfo)
	    else return resolved


handleConflict :: FC.ConflictInfo -> IO ()
handleConflict confInfo = do
   let origFP = FC.origFilePath confInfo
   exists <- doesFileExist origFP
   if not exists
      then putStrLn $ "Found conflicts for the file '" ++ origFP ++ "', but the file itself is missing! Skipping it."
      else do
	 putConfInfo confInfo
	 askUser confInfo


putConfInfo :: FC.ConflictInfo -> IO ()
putConfInfo confInfo = do
   putStrLn $ "\nConflicting file: " ++ FC.origFilePath confInfo
   void $ foldlM (\i c -> putConf i c >> (return $ i+1)) 1 (FC.conflicts confInfo)
   where
      putConf idx conf =
	 putStrLn $ "   (" ++ [intToDigit idx] ++ ") " ++ FC.details conf


askUser confInfo = do
   putStr "\n(T)ake File (NUM) | (M)ove to Trash | Show (D)iff (NUM [NUM]) | (S)kip | (Q)uit | (H)elp: " 
   line <- getLine
   let confs        = FC.conflicts confInfo
       numConfs     = length confs
       validD       = validDigit 1 numConfs
       invalidTake  = putStrLn $ "Invalid input! Use e.g: 't1'"
       invalidDiff  = putStrLn $ "Invalid input! Use e.g: 'd1' or 'd12'"
       invalidInput = putStrLn $ "Invalid input! See Help"
       askAgain     = askUser confInfo
   case map toUpper line of
        ('T':d:[]) | validD d  -> takeFile (digitToInt d) confInfo
	           | otherwise -> invalidTake >> askAgain

        ('M':[]) -> mapM_ (\c -> moveToTrash $ FC.filePath c) confs  

        ('D':[]) | numConfs == 1 -> do showDiff (FC.origFilePath confInfo) 
	                                        (FC.filePath $ confs !! 0) 
				       askAgain      

	         | otherwise     -> invalidInput >> askAgain

        ('D':d:[]) | validD d  -> do let i = (digitToInt d) - 1 
				     showDiff (FC.origFilePath confInfo) 
				              (FC.filePath $ confs !! i)
				     askAgain

	           | otherwise -> invalidDiff >> askAgain

        ('D':d1:d2:[]) | validD d1 && validD d2 -> do let i1 = (digitToInt d1) - 1
						          i2 = (digitToInt d2) - 1
						      showDiff (FC.filePath $ confs !! i1) 
						               (FC.filePath $ confs !! i2)
						      askAgain

		       | otherwise -> invalidDiff >> askAgain

        ('S':[])  -> return ()
	('Q':[])  -> exitSuccess
	('H':[])  -> printRuntineHelp >> askAgain
	('?':[])  -> printRuntineHelp >> askAgain
	otherwise -> invalidInput >> askAgain

   where
      validDigit min max d = isDigit d && let i = digitToInt d in i >= min && i <= max

      moveToTrash file =
         errorsToStderr $ do 
  	  trashDir <- trashDirectory
  	  createDirectoryIfMissing True trashDir
  	  let (dir, fileName) = splitFileName file
  	  copyFile file (trashDir </> fileName) 
  	  removeFile file

      takeFile num confInfo = do
         (year, month, day) <- getCurrentDate
         let idx        = num - 1
	     confs      = FC.conflicts confInfo
             file       = FC.filePath $ confs !! idx
  	     origFile   = FC.origFilePath confInfo
  	     origBackup = FC.dir confInfo </> FC.fileName confInfo 
	                  ++ "_backup_" ++ show year ++ "-" ++ show month
			  ++ "-" ++ show day ++ FC.suffix confInfo

         errorsToStderr $ do
	    copyFile origFile origBackup
            moveToTrash origBackup
            copyFile file origFile
            mapM_ (\c -> moveToTrash (FC.filePath c)) confs

      showDiff file1 file2 = do
         putStrLn ""
         diff <- getEnvOrDefault "CONFSOLVE_DIFF" defaultDiff
         handle <- runCommand $ diff ++ " " ++ quote file1 ++ " " ++ quote file2
         waitForProcess handle
         return ()

      quote string = "\"" ++ string ++ "\""
