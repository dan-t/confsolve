{-# LANGUAGE ScopedTypeVariables, OverloadedStrings #-}

import System.IO
import System.IO.Error (IOErrorType)
import System.Environment
import System.Process
import System.Exit
import Data.Char (intToDigit, digitToInt, isDigit, toUpper)
import Data.Foldable (foldrM, foldlM)
import qualified Data.HashMap.Strict as HM
import qualified Filesystem as FS
import qualified Data.Text as T
import qualified Filesystem.Path.CurrentOS as FP
import Filesystem.Path.CurrentOS ((</>))
import Control.Monad (mapM_, when)
import Control.Applicative ((<$>))
import Foreign.Marshal.Error (void)
import qualified FileConflict as FC
import qualified Dropbox.Conflict as DB
import qualified Wuala.Conflict as WU
import Utils
import ConfsolveArgs


main = do
   hSetBuffering stdout NoBuffering
   hSetBuffering stdin  NoBuffering
   args <- confsolveArgs
   let dir = FP.fromText $ T.pack (directory args)
   when (dropbox args) $
      resolveConflicts DB.Parser dir

   when (wuala args) $
      resolveConflicts WU.Parser dir


printRuntineHelp = do
   trashDir <- show <$> trashDirectory
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

type Conflicts = HM.HashMap T.Text [FC.ConflictingFile]


resolveConflicts :: FC.ConflictParser parser => parser -> FP.FilePath -> IO ()
resolveConflicts parser filePath = do
   conflicts <- collectConflicts parser filePath HM.empty
   mapM_ (handleConflict . conflict) (HM.toList conflicts)
   where
      conflict (path, files) = FC.Conflict (FP.fromText path) files


collectConflicts :: FC.ConflictParser parser => parser -> FP.FilePath -> Conflicts -> IO Conflicts
collectConflicts parser filePath conflicts = do
   isDir <- FS.isDirectory filePath
   if isDir
      then do
         entries <- FS.listDirectory filePath
         foldrM (\entry conflicts -> collectConflicts parser entry conflicts) conflicts entries
      else do
         isFile <- FS.isFile filePath
         if isFile
            then do
               let bname = toText $ FP.basename filePath
               case FC.parseConflict parser bname of
                    Just (realBaseName, details) -> do
                       return $ HM.insertWith (++)
                                              (toText $ replaceBaseName filePath (FP.fromText realBaseName))
                                              [FC.ConflictingFile details filePath]
                                              conflicts

                    Nothing -> return conflicts

            else return conflicts


handleConflict :: FC.Conflict -> IO ()
handleConflict conflict = do
   let origFP = FC.origFilePath conflict
   exists <- FS.isFile origFP
   if not exists
      then putStrLn $ "Found conflicts for the file '" ++ show origFP ++ "', but the file itself is missing! Skipping it."
      else do
	 putConflict conflict
	 askUser conflict


putConflict :: FC.Conflict -> IO ()
putConflict conflict = do
   putStrLn $ "\nConflicting file: " ++ (show $ FC.origFilePath conflict)
   void $ foldlM (\i c -> putConf i c >> (return $ i+1)) 1 (FC.conflictingFiles conflict)
   where
      putConf idx conf =
	 putStrLn $ "   (" ++ [intToDigit idx] ++ ") " ++ (show $ FC.details conf)


askUser conflict = do
   putStr "\n(T)ake File (NUM) | (M)ove to Trash | Show (D)iff (NUM [NUM]) | (S)kip | (Q)uit | (H)elp: "
   line <- getLine
   let confs        = FC.conflictingFiles conflict
       numConfs     = length confs
       validD       = validDigit 1 numConfs
       invalidTake  = putStrLn $ "Invalid input! Use e.g: 't1'"
       invalidDiff  = putStrLn $ "Invalid input! Use e.g: 'd1' or 'd12'"
       invalidInput = putStrLn $ "Invalid input! See Help"
       askAgain     = askUser conflict
   case map toUpper line of
        ('T':d:[]) | validD d  -> takeFile (digitToInt d) conflict
	           | otherwise -> invalidTake >> askAgain

        ('M':[]) -> mapM_ (\c -> moveToTrash $ FC.filePath c) confs

        ('D':[]) | numConfs == 1 -> do showDiff (FC.origFilePath conflict)
	                                        (FC.filePath $ confs !! 0)
				       askAgain

	         | otherwise     -> invalidInput >> askAgain

        ('D':d:[]) | validD d  -> do let i = (digitToInt d) - 1
				     showDiff (FC.origFilePath conflict)
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

      moveToTrash filePath =
         errorsToStderr $ do
  	  trashDir <- trashDirectory
          FS.createTree trashDir
          let fileName = FP.filename filePath
          FS.copyFile filePath (trashDir </> fileName)
          FS.removeFile filePath

      takeFile num conflict = do
         (year, month, day) <- getCurrentDate
         let idx            = num - 1
	     confs          = FC.conflictingFiles conflict
             file           = FC.filePath $ confs !! idx
             origFile       = FC.origFilePath conflict
             backupSpec     = T.pack $ "_backup_" ++ show year ++ "-" ++ show month ++ "-" ++ show day
             backupBaseName = (toText $ FP.basename origFile) `T.append` backupSpec
             backupFile     = replaceBaseName origFile (FP.fromText backupBaseName)

         errorsToStderr $ do
	    FS.copyFile origFile backupFile
            moveToTrash backupFile
            FS.copyFile file origFile
            mapM_ (\c -> moveToTrash (FC.filePath c)) confs

      showDiff file1 file2 = do
         putStrLn ""
         diff <- getEnvOrDefault "CONFSOLVE_DIFF" defaultDiff
         handle <- runCommand $ diff ++ " " ++ (quote $ toString file1) ++ " " ++ (quote $ toString file2)
         waitForProcess handle
         return ()

      quote string = "\"" ++ string ++ "\""
