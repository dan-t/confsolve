module Dropbox where
import Data.List
import System.FilePath
import Text.Regex.Posix
import FileConflict
import Utils

data DropboxConflict = DropboxConflict

instance FileConflict DropboxConflict where
   find fc file = do
      let match = concat (file =~ pathRegex :: [[String]])
      if length match == 6
	 then do
	    let (_:dir:fileName:_:_:suffix:[]) = match
	    confInfo <- conflictInfo dir fileName suffix
	    return $ Just confInfo
	 else return Nothing

conflictInfo dir fileName suffix = do
   conts <- getDirContents dir
   let confs = filterConfs conts []
   return $ ConflictInfo dir fileName suffix confs
   where
      filterConfs []     confs = confs
      filterConfs (c:cs) confs =
	 let match = concat (c =~ fileRegex :: [[String]])
	     in if length match == 5
		   then let (_:fname:host:date:suff:[]) = match
			    in if fname == fileName && suff == suffix
			          then let conf = Conflict (host ++ " from " ++ date) (dir </> c)
				           in filterConfs cs (conf : confs)
				  else filterConfs cs confs
		   else filterConfs cs confs

fileRegex = "(.*) \\((.*) conflicted copy (.*)\\)(.*)"
pathRegex = "(.*)" </> fileRegex
