module Wuala where
import Data.List
import System.FilePath
import Text.Regex.Posix
import FileConflict
import Utils

data WualaConflict = WualaConflict

instance FileConflict WualaConflict where
   find fc file = do
      if hasConflict file
	 then do
	    let (_:dir:fileName:_:_:suffix:_) = concat (file =~ pathRegex :: [[String]])
	    confInfo <- conflictInfo dir fileName suffix
	    return $ Just confInfo
	 else return Nothing
	 

hasConflict file = "conflicting version" `isInfixOf` file
fileRegex = "(.*) \\(conflicting version (.*) from (.*)\\)(.*)"
pathRegex = "(.*)" </> fileRegex

conflictInfo dir fileName suffix = do
   conts <- getDirContents dir
   let confs = filter (\c -> hasConflict c && fileName `isPrefixOf` c) conts
   return $ ConflictInfo dir fileName suffix (conflicts dir confs)
   where
      conflicts dir confs = map (\c -> conflict dir c) confs 
      conflict dir conf =
	 let (_:_:version:host:_:[]) = concat (conf =~ fileRegex :: [[String]])
	     in Conflict ("version " ++ version ++ " from " ++ host) (dir </> conf)
