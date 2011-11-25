module FileConflict where
import System.FilePath
import Data.Maybe

-- the details and filePath of one conflicting file
data Conflict = Conflict {
   details  :: String,
   filePath :: FilePath } deriving (Show)

-- the info of all conflicting files
data ConflictInfo = ConflictInfo {
   dir       :: String,
   fileName  :: String,
   suffix    :: String,
   conflicts :: [Conflict] } deriving (Show)

origFilePath :: ConflictInfo -> FilePath
origFilePath ci = dir ci </> fileName ci ++ suffix ci

class FileConflict a where
   find :: a -> FilePath -> IO (Maybe ConflictInfo)
