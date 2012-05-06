module Wuala where
import Data.List
import System.FilePath
import Text.Regex.PCRE
import FileConflict
import Utils

data WualaConflict = WualaConflict

instance FileConflict WualaConflict where
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
		   then let (_:fname:version:host:suff:[]) = match
			    in if fname == fileName && suff == suffix
				  then let conf = Conflict ("version " ++ version ++ " from " ++ host) (dir </> c)
			                   in filterConfs cs (conf : confs)
				  else filterConfs cs confs
		   else filterConfs cs confs

fileRegex = "(.*) \\(conflicting version (.*) from (.*)\\)(.*)"
pathRegex = "(.*)" </> fileRegex
