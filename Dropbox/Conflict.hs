module Dropbox.Conflict where
import Data.List
import System.FilePath
import FileConflict
import Control.Applicative ((<$>))
import qualified Dropbox.FileNameParser as FNP
import qualified Data.Text as T
import Utils

data DropboxConflict = DropboxConflict

instance FileConflict DropboxConflict where
   find fc file =
      case FNP.parse (T.pack fileName) of
           Just (FNP.FileInfo fn ver host) -> Just <$> conflictInfo dir (T.unpack fn) ext
           Nothing                         -> return Nothing
      where
         (dir, fileName, ext) = splitFile file


splitFile file = (dir, fileName, ext)
   where
      (fileName, ext)        = splitExtensions fileNameWithExt
      (dir, fileNameWithExt) = splitFileName file


conflictInfo dir fileName suffix = do
   conts <- getDirContents dir
   return $ ConflictInfo dir fileName suffix (filterConfs conts [])
   where
      filterConfs []     confs = confs
      filterConfs (c:cs) confs
         | suffix /= ext = filterConfs cs confs
         | otherwise =
            case FNP.parse (T.pack fn) of
                 Just (FNP.FileInfo f h d) ->
                    if fileName == T.unpack f
                       then let conf = Conflict ("Version " ++ T.unpack d ++ " from " ++ T.unpack h) (dir </> c)
                                in filterConfs cs (conf : confs)
                       else filterConfs cs confs

                 Nothing -> filterConfs cs confs
         where
            (fn, ext) = splitExtensions c
