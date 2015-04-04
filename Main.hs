{-# LANGUAGE ScopedTypeVariables, OverloadedStrings, FlexibleContexts #-}

import System.IO
import System.IO.Error (IOErrorType)
import System.Environment
import System.Process
import System.Exit
import Data.Foldable (foldrM, foldlM)
import qualified Data.HashMap.Strict as HM
import qualified Filesystem as FS
import qualified Data.Text as T
import qualified Filesystem.Path.CurrentOS as FP
import Filesystem.Path.CurrentOS ((</>))
import Control.Monad (mapM_, when, void)
import Control.Applicative ((<$>))
import qualified FileConflict as FC
import qualified Dropbox.Conflict as DB
import qualified Wuala.Conflict as WU
import Utils
import ConfsolveArgs
import ParseInput


main :: IO ()
main = do
   hSetBuffering stdout NoBuffering
   hSetBuffering stdin  NoBuffering
   args <- confsolveArgs
   let dir = FP.fromText $ T.pack (directory args)
   when (dropbox args) $
      resolveConflicts DB.parse dir

   when (wuala args) $
      resolveConflicts WU.parse dir


printRuntineHelp :: IO ()
printRuntineHelp = do
   trashDir <- show <$> trashDirectory
   putStrLn $ ""
   putStrLn $ "Runtime Options:"
   putStrLn $ "   (T)ake File (NUM) => By pressing 't' and a number (e.g 't1'), the conflicting file with the"
   putStrLn $ "                        number NUM is used as the new version. A copy of the"
   putStrLn $ "                        current file and the other conflicting files is put"
   putStrLn $ "                        into the trash directory '" ++ trashDir ++ "'."
   putStrLn $ ""
   putStrLn $ "   (M)ove to Trash   => By pressing 'm', all conflicting files are"
   putStrLn $ "                        moved into the trash directory '" ++ trashDir ++ "'."
   putStrLn $ ""
   putStrLn $ "   Show (D)iff (NUM) => By pressing 'd' and a number (e.g 'd1'), the difference between the"
   putStrLn $ "                        current file and the conflicting file NUM is shown."
   putStrLn $ "                        If there's only one conflicting file, then only pressing"
   putStrLn $ "                        'd' is sufficient."
   putStrLn $ "                        By pressing 'd' and two numbers (e.g 'd1 2'), the difference between"
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


resolveConflicts :: FC.ConflictParser -> FP.FilePath -> IO ()
resolveConflicts parse filePath = do
   conflicts <- collectConflicts parse filePath HM.empty
   mapM_ (handleConflict . conflict) (HM.toList conflicts)
   where
      conflict (path, files) = FC.Conflict (FP.fromText path) files


collectConflicts :: FC.ConflictParser -> FP.FilePath -> Conflicts -> IO Conflicts
collectConflicts parse filePath conflicts = do
   isDir <- FS.isDirectory filePath
   if isDir
      then do
         entries <- FS.listDirectory filePath
         foldrM (\entry conflicts -> collectConflicts parse entry conflicts) conflicts entries
      else do
         isFile <- FS.isFile filePath
         if isFile
            then do
               let bname = toText $ FP.basename filePath
               case parse bname of
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
	 askUserTillHandled conflict
   where
      askUserTillHandled confict = do
         reply    <- askUser confict
         askAgain <- maybe (putStrLn "Invalid input! See Help" >> return True)
                           (handleUserReply confict)
                           reply
         when askAgain $ do
            putConflict confict
            askUserTillHandled confict


type AskAgain = Bool
handleUserReply :: FC.Conflict -> UserReply -> IO AskAgain
handleUserReply conflict reply =
   case reply of
      TakeFile num -> do
         takeFile num conflict
         dontAskAgain

      MoveToTrash -> do
         mapM_ (\c -> moveToTrash $ FC.filePath c) confs
         dontAskAgain

      ShowDiff -> do
         showDiff (FC.origFilePath conflict) (FC.filePath $ confs !! 0)
         askAgain

      ShowDiffWith num -> do
         showDiff (FC.origFilePath conflict) (FC.filePath $ confs !! (num - 1))
         askAgain

      ShowDiffBetween num1 num2 -> do
         showDiff (FC.filePath $ confs !! (num1 - 1)) (FC.filePath $ confs !! (num2 - 1))
         askAgain

      Skip -> dontAskAgain
      Quit -> exitSuccess
      Help -> printRuntineHelp >> askAgain
   where
      confs        = FC.conflictingFiles conflict
      askAgain     = return True
      dontAskAgain = return False

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


putConflict :: FC.Conflict -> IO ()
putConflict conflict = do
   putStrLn $ "\nConflicting file: " ++ (show $ FC.origFilePath conflict)
   void $ foldlM (\i c -> putConf i c >> (return $ i+1)) 1 (FC.conflictingFiles conflict)
   where
      putConf fileNum conf =
	 putStrLn $ "   (" ++ show fileNum ++ ") " ++ (show $ FC.details conf)


askUser :: FC.Conflict -> IO (Maybe UserReply)
askUser conflict = do
   putStr "\n(T)ake File (NUM) | (M)ove to Trash | Show (D)iff (NUM [NUM]) | (S)kip | (Q)uit | (H)elp: "
   line <- getLine
   return $ parseInput line (length . FC.conflictingFiles $ conflict)
