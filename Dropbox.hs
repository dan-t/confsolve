module Dropbox where
import Data.List
import System.FilePath
import Text.Regex.Posix
import FileConflict
import Utils

data DropboxConflict = DropboxConflict

instance FileConflict DropboxConflict where
   find fc file = do
      if hasConflict file
	 then do
	    let (_:dir:fileName:_:_:suffix:_) = concat (file =~ pathRegex :: [[String]])
	    confInfo <- conflictInfo dir fileName suffix
	    return $ Just confInfo
	 else return Nothing
	 

hasConflict file = "conflicted copy" `isInfixOf` file
fileRegex = "(.*) \\((.*) conflicted copy (.*)\\)(.*)"
pathRegex = "(.*)" </> fileRegex

conflictInfo dir fileName suffix = do
   conts <- getDirContents dir
   let confs = filter (\c -> hasConflict c && fileName `isPrefixOf` c) conts
   return $ ConflictInfo dir fileName suffix (conflicts confs)
   where
      conflicts confs = map (\c -> conflict c) confs 
      conflict conf =
	 let (_:_:host:date:_:[]) = concat (conf =~ fileRegex :: [[String]])
	     in Conflict (host ++ " from " ++ date) conf
